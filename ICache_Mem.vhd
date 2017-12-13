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
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
--library techmap;
--use techmap.gencomp.all;
library work;
use work.Constants.all;
use work.icache_pkg.all;

entity ICache_Mem is
  generic(IC_Tag_Size       : natural := IC_Tag_Size;
          IC_Index_in_Bits  : natural := IC_Index_in_Bits;
          IC_Num_Data_Mem   : natural := IC_Num_Data_Mem;
          IC_Data_Mem_Width : natural := IC_Data_Mem_Width;
          IMem2DataWRatio : natural := IMem2DataWRatio);         
  port (clk            : in  std_logic;
        Reset          : in  std_logic;  -- active low
        ICache_Mem_in  : in  ICache_Mem_in_type;
        ICache_Mem_out : out ICache_Mem_out_type);
end ICache_Mem;


architecture ICache_Mem_behave of ICache_Mem is
  
  signal Tag_Mem_in         : Tag_Mem_in_type;
  signal Tag_Mem_out        : Tag_Mem_out_type;
  type   Data_Mem_in_array_type is array (IC_Num_Data_Mem-1 downto 0) of Data_Mem_in_type;
  signal Data_Mem_in_array  : Data_Mem_in_array_type;
  type   Data_Mem_out_array_type is array (IC_Num_Data_Mem-1 downto 0) of Data_Mem_out_type;
  signal Data_Mem_out_array : Data_Mem_out_array_type;


begin
  
  Tag_Mem_in <= (Tag_en    => ICache_Mem_in.en_cache_Tag,
                 Address   => ICache_Mem_in.Set_Addr,
                 Write_tag => ICache_Mem_in.write_cache_Tag,
                 DataI     => ICache_Mem_in.Tag_DataI);

  MyIC_Tag_Mem : IC_Tag_Mem
    generic map(IC_Index_in_Bits,
                IC_Tag_Size)
    port map(clk         => clk,
             Tag_Mem_in  => Tag_Mem_in,
             Tag_Mem_out => Tag_Mem_out);
  
--An example of technology specific mapping.
--( "syncram" from grlib is used, can be downloaded from Gaisler website )
--  MyIC_tag_Syncram : syncram
--    generic map(tech  => Virtex6,
--                abits => IC_Index_in_Bits,
--                dbits => (IC_Tag_Size+1))
--    port map(
--      clk     => clk,
--      address => Tag_Mem_in.Address,
--      datain  => Tag_Mem_in.DataI,
--      dataout => Tag_Mem_out.DataO,
--      enable  => Tag_Mem_in.Tag_en,
--      write   => Tag_Mem_in.Write_tag);


  Data_Mem_Inst : for i in 0 to IC_Num_Data_Mem-1 generate
  begin
    Data_Mem_in_array(i) <= (Data_en    => ICache_Mem_in.en_cache_Data(i),
                             Address    => ICache_Mem_in.Set_Addr,
                             Write_Data => ICache_Mem_in.write_cache_Data(i),
                             DataI      => ICache_Mem_in.IMem_Data(((i mod IMem2DataWRatio)+1)*IC_Data_Mem_Width-1 downto (i mod IMem2DataWRatio)*IC_Data_Mem_Width));

    
    ICache_Mem_out.ICache_DataO_Block((i+1)*IC_Data_Mem_Width-1 downto i*IC_Data_Mem_Width) <= Data_Mem_out_array(i).DataO;

--   MyIC_Data_Syncram : syncram
--      generic map(tech  => Virtex6,
--                  abits => IC_Index_in_Bits,
--                  dbits => (IC_Data_Mem_Width))
--      port map(
--        clk     => clk,
--        address => Data_Mem_in_array(i).Address,
--        datain  => Data_Mem_in_array(i).DataI,
--        dataout => Data_Mem_out_array(i).DataO,
--        enable  => Data_Mem_in_array(i).Data_en,
--        write   => Data_Mem_in_array(i).Write_Data);

--An example of technology specific mapping.
--( "syncram" from grlib is used, can be downloaded from Gaisler website )
    MyIC_Data_Mem : IC_Data_Mem
      generic map(IC_Index_in_Bits,
                  IC_Data_Mem_Width)
      port map( clk          => clk,
                Data_Mem_in  => Data_Mem_in_array(i),
                Data_Mem_out => Data_Mem_out_array(i));

  end generate;

  ICache_Mem_out.hit <= '1' when (Tag_Mem_out.DataO = ICache_Mem_in.Tag_DataI) else '0';
  
end ICache_Mem_behave;
