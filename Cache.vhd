-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                Configurable RTL Model for Level-1 Caches                  --
--                                                                           --
-- This file is distributed under the license terms given by LICENSE.TXT     --
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Main author: Vahid Saljooghi                                              --
-- Project leaders: Alen Bardizbanyan, Magnus Sjalander, Per Larsson-Edefors --
--                                                                           --
-- For more information on the project, consult the following paper          --
-- "Configurable RTL Model for Level-1 Caches",                              --
-- V. Saljooghi, A. Bardizbanyan, M. Sjalander and P. Larsson-Edefors,       --
-- Proceedings of NORCHIP, Copenhagen, Denmark, Nov. 11-12, 2012             --
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
library work;
use work.icache_pkg.all;
use work.dcache_pkg.all;
use work.cache_pkg.all;
use work.arbiter_pkg.all;

entity cache is
  generic (
    IC_Block_Size           : natural;
    Address_Space_CPU       : natural;
    Address_Space_IMem      : natural;
    Instr_Size              : natural;
    IC_Offset_in_bits       : natural;
    IC_Tag_Size             : natural;
    IC_Index_in_Bits        : natural;
    IC_Num_Data_Mem         : natural;
    IC_Data_Mem_Width       : natural;
    IMem2DataWRatio         : natural;
    Num_IMem_Refer          : natural;
    IC_LRU_Table_Width      : natural;
    IC_Num_Of_Ways          : natural;
    IC_Num_Of_Sets          : natural;
    IC_Rep_Policy           : natural;
    IC_LRU                  : natural;
    DC_Block_Size           : natural;
    Address_Space_DMem      : natural;
    Word_Size               : natural;
    DMemWidth               : natural;
    DC_Block_Offset_in_bits : natural;
    DC_Tag_Size             : natural;
    DC_Index_in_Bits        : natural;
    DC_Num_Data_Mem         : natural;
    DC_Data_Mem_Width       : natural;
    DMem2DataWRatio         : natural;
    Num_DMem_Refer          : natural;
    DC_LRU_Table_Width      : natural;
    DC_Num_Of_Ways          : natural;
    DC_Num_Of_Sets          : natural;
    DC_Rep_Policy           : natural;
    DC_LRU                  : natural);
  port(
    Clk     : in  std_logic;
    Reset   : in  std_logic;
    ic_in   : in  ic_in_record;
    ic_out  : out ic_out_record;
    dc_in   : in  dc_in_record;
    dc_out  : out dc_out_record;
    mem_in  : in  mem_in_record;
    mem_out : out mem_out_record);
end cache;

architecture rtl of cache is
  signal ICache_in      : ICache_in_type;
  signal ICache_out     : ICache_out_type;
  signal Hazard_Stall   : std_logic;
  signal DCache_in      : DCache_in_type;
  signal DCache_out     : DCache_out_type;
  signal arbiter_ic_in  : arbiter_ic_in_record;
  signal arbiter_ic_out : arbiter_ic_out_record;
  signal arbiter_dc_in  : arbiter_dc_in_record;
  signal arbiter_dc_out : arbiter_dc_out_record;
begin
  ICache_in.PC              <= ic_in.addr;
  ICache_in.IFetch_Stopped  <= ic_in.stall or ((not(ICache_out.hit)) or (not(DCache_out.ready_DCache)));
  ICache_in.Extr_Stall      <= ic_in.stall or (not DCache_out.ready_DCache);
  ic_out.data               <= ICache_out.ICache_DataO_Instr;
  ic_out.stall              <= not(ICache_out.hit);

  DCache_in.DFetch_Stopped   <= (not(ICache_out.hit)) or (not(DCache_out.ready_DCache));
  DCache_in.CPU_Re          <= dc_in.rd;
  DCache_in.CPU_Wr          <= dc_in.wr;
  DCache_in.CPU_Data        <= dc_in.data;
  DCache_in.ALU             <= dc_in.addr;
  DCache_in.MaskI           <= dc_in.mask;
  dc_out.data               <= DCache_out.DCache_DataO_Word;
  dc_out.stall              <= not(DCache_out.ready_DCache);
  
  DCache_in.Extr_Stall      <= not(ICache_out.hit);
  DCache_in.DFetch_Stopped   <= (not(ICache_out.hit)) or (not(DCache_out.ready_DCache));
  
  arbiter_dc_in.rd          <= DCache_out.Re_DMem;
  arbiter_dc_in.wr          <= DCache_out.Wr_DMem;
  arbiter_ic_in.rd          <= ICache_out.Re_IMem;
  arbiter_dc_in.acc         <= DCache_out.Mem_Acc_Finished;
  arbiter_ic_in.acc         <= ICache_out.Mem_Acc_Finished;  

  arbiter_dc_in.addr        <= DCache_out.Address2DMem;
  arbiter_ic_in.addr        <= ICache_out.Address2IMem;
  arbiter_dc_in.data        <= DCache_out.Data2DMem;
  arbiter_dc_in.mask        <= DCache_out.MaskO(3) & DCache_out.MaskO(3) & DCache_out.MaskO(3) & DCache_out.MaskO(3) & DCache_out.MaskO(3) & DCache_out.MaskO(3) & DCache_out.MaskO(3) & DCache_out.MaskO(3) &
                DCache_out.MaskO(2) & DCache_out.MaskO(2) & DCache_out.MaskO(2) & DCache_out.MaskO(2) & DCache_out.MaskO(2) & DCache_out.MaskO(2) & DCache_out.MaskO(2) & DCache_out.MaskO(2) &
                DCache_out.MaskO(1) & DCache_out.MaskO(1) & DCache_out.MaskO(1) & DCache_out.MaskO(1) & DCache_out.MaskO(1) & DCache_out.MaskO(1) & DCache_out.MaskO(1) & DCache_out.MaskO(1) &
                DCache_out.MaskO(0) & DCache_out.MaskO(0) & DCache_out.MaskO(0) & DCache_out.MaskO(0) & DCache_out.MaskO(0) & DCache_out.MaskO(0) & DCache_out.MaskO(0) & DCache_out.MaskO(0);
  
  DCache_in.DMem_Data       <= arbiter_dc_out.data;
  ICache_in.IMem_Data       <= arbiter_ic_out.data;
  DCache_in.ready_DMem      <= arbiter_dc_out.ready;
  ICache_in.ready_IMem      <= arbiter_ic_out.ready;

  arbiter_1: arbiter
    port map (
      Clk             => Clk,
      Reset           => Reset,
      arbiter_ic_in   => arbiter_ic_in,
      arbiter_ic_out  => arbiter_ic_out,
      arbiter_dc_in   => arbiter_dc_in,
      arbiter_dc_out  => arbiter_dc_out,
      arbiter_mem_in  => mem_in,
      arbiter_mem_out => mem_out);
  
  MyICache : ICache
    generic map (
      IC_Block_Size,
      Address_Space_CPU,
      Address_Space_IMem,
      Instr_Size,
      IC_Offset_in_bits,
      IC_Tag_Size,
      IC_Index_in_Bits,
      IC_Num_Data_Mem,
      IC_Data_Mem_Width,
      IMem2DataWRatio,
      Num_IMem_Refer,
      IC_LRU_Table_Width,
      IC_Num_Of_Ways,
      IC_Num_Of_Sets,
      IC_Rep_Policy,
      IC_LRU)
    port map(
      clk        => clk,
      Reset      => Reset,
      ICache_in  => ICache_in,
      ICache_out => ICache_out);            

  MyDCache : DCache
    generic map(
      DC_Block_Size,
      Address_Space_CPU,
      Address_Space_DMem,
      Word_Size,
      DMemWidth,
      DC_Block_Offset_in_bits,
      DC_Tag_Size,
      DC_Index_in_Bits,
      DC_Num_Data_Mem,
      DC_Data_Mem_Width,
      DMem2DataWRatio,
      Num_DMem_Refer,
      DC_LRU_Table_Width,
      DC_Num_Of_Ways,
      DC_Num_Of_Sets,
      DC_Rep_Policy,
      DC_LRU)
    port map(
      clk        => clk,
      Reset      => Reset,
      DCache_in  => DCache_in,
      DCache_out => DCache_out);
end rtl;
