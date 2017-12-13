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
use work.Constants.all;

package cache_pkg is
  -- Records for communicating between CPU and IC
  type ic_in_record is record
    addr  : std_logic_vector (Address_Space_CPU-1 downto 0);
    stall : std_logic;
  end record;

  type ic_out_record is record
    data  : std_logic_vector (Instr_Size-1 downto 0);
    stall : std_logic;
  end record;
  
  -- Records for communicating between CPU and DC
  type dc_in_record is record
    addr  : std_logic_vector (Data_Bus_Width-1 downto 0);
    data  : std_logic_vector (Data_Bus_Width-1 downto 0);
    mask  : std_logic_vector (DC_Bytes_in_Word-1 downto 0);
    rd    : std_logic;
    wr    : std_logic;
  end record;

  type dc_out_record is record
    data  : std_logic_vector (Word_Size-1 downto 0);
    stall : std_logic;
  end record;

  -- Records for communicating between Arbiter and Memory
  type mem_in_record is record
    data  : std_logic_vector(DMemWidth-1 downto 0);    
    stall : std_logic;
  end record;

  type mem_out_record is record
    addr : std_logic_vector(Address_Space_IMem-1 downto 0);
    data : std_logic_vector(DMemWidth-1 downto 0);
    mask : std_logic_vector(DMemWidth-1 downto 0);
    rd   : std_logic;
    wr   : std_logic;
  end record;

  component cache is
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
  end component cache;

end cache_pkg;
