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
use work.dcache_pkg.all;

entity DCache_Mem is
  generic(DC_Tag_Size          : natural := DC_Tag_Size;
          DC_Index_in_Bits     : natural := DC_Index_in_Bits;
          DC_Num_Data_Mem      : natural := DC_Num_Data_Mem;
          DC_Data_Mem_Width    : natural := DC_Data_Mem_Width);         
  port (clk            : in  std_logic;
        Reset          : in  std_logic;  -- active low
        DCache_Mem_in  : in  DCache_Mem_in_type;
        DCache_Mem_out : out DCache_Mem_out_type);
end DCache_Mem;


architecture DCache_Mem_behave of DCache_Mem is  
  signal Tag_Mem_in         : Tag_Mem_in_type;
  signal Tag_Mem_out        : Tag_Mem_out_type;
  type   Data_Mem_in_array_type is array (DC_Num_Data_Mem-1 downto 0) of Data_Mem_in_type;
  signal Data_Mem_in_array  : Data_Mem_in_array_type;
  type   Data_Mem_out_array_type is array (DC_Num_Data_Mem-1 downto 0) of Data_Mem_out_type;
  signal Data_Mem_out_array : Data_Mem_out_array_type;
  signal DirtBit_Mem_in     : DirtBit_Mem_in_type;
  signal DirtBit_Mem_out     : DirtBit_Mem_out_type;

begin
  Tag_Mem_in <= (Tag_en    => DCache_Mem_in.en_cache_Tag,
                 Address   => DCache_Mem_in.Set_Addr,
                 Write_tag => DCache_Mem_in.write_cache_Tag, 
                 DataI     => DCache_Mem_in.Tag_DataI);

--An example of technology specific mapping.
--( "syncram" from grlib is used, can be downloaded from Gaisler website )
--  MyDC_tag_Syncram : syncram
--    generic map(tech  => Virtex6,
--                abits => DC_Index_in_Bits,
--                dbits => (DC_Tag_Size+1))
--    port map(
--      clk     => clk,
--      address => Tag_Mem_in.Address,
--      datain  => Tag_Mem_in.DataI,
--      dataout => Tag_Mem_out.DataO,
--      enable  => Tag_Mem_in.Tag_en,
--      write   => Tag_Mem_in.Write_tag);

  MyDC_Tag_Mem : DC_Tag_Mem
    generic map(DC_Index_in_Bits,
                DC_Tag_Size)
    port map(clk          => clk,
             Tag_Mem_in  => Tag_Mem_in,
             Tag_Mem_out => Tag_Mem_out);

  DCache_Mem_out.Tag_DataO<=Tag_Mem_out.DataO(DC_Tag_Size-1 downto 0);

DC_Data_Mem_Inst : for i in 0 to DC_Num_Data_Mem-1 generate
  begin
    Data_Mem_in_array(i) <= (Data_en    => DCache_Mem_in.en_cache_Data(i),
                             Address    => DCache_Mem_in.Set_Addr,
                             Write_Data => DCache_Mem_in.write_cache_Data(i), 
                             DataI      => DCache_Mem_in.DCache_DataI_Block(i));


--An example of technology specific mapping.
--( "syncram" from grlib is used, can be downloaded from Gaisler website )    
--    MyDC_Data_Syncram : syncram
--      generic map(tech  => Virtex6,
--                  abits => DC_Index_in_Bits,
--                  dbits => (DC_Data_Mem_Width))
--      port map(
--        clk     => clk,
--        address => Data_Mem_in_array(i).Address,
--        datain  => Data_Mem_in_array(i).DataI,
--        dataout => Data_Mem_out_array(i).DataO,
--        enable  => Data_Mem_in_array(i).Data_en,
--        write   => Data_Mem_in_array(i).Write_Data);

    MyDC_Data_Mem : DC_Data_Mem
      generic map(DC_Index_in_Bits,
                  DC_Data_Mem_Width)
      port map( clk          => clk,
                Data_Mem_in  => Data_Mem_in_array(i),
                Data_Mem_out => Data_Mem_out_array(i));

    DCache_Mem_out.DCache_DataO_Block((i+1)*DC_Data_Mem_Width-1 downto i*DC_Data_Mem_Width) <= Data_Mem_out_array(i).DataO;

end generate;
   
DirtBit_Mem_gen:if(DC_Write_Policy=WrBackAlloc) generate              
   DirtBit_Mem_in<= (en        => DCache_Mem_in.en_DirtBit,                          
                     Address   => DCache_Mem_in.Set_Addr ,                          
                     DataI     => DCache_Mem_in.DirtBit_DataI);
                
   MyDirtBit_Mem : DirtBit_Mem
    generic map(DC_Index_in_Bits)          
    port map( clk                    =>clk,
           Reset                     =>Reset,
           DirtBit_Mem_in            => DirtBit_Mem_in,        
           DirtBit_Mem_out           => DirtBit_Mem_out);             

  DCache_Mem_out.DirtBit_DataO<=DirtBit_Mem_out.DataO;
end generate;

  DCache_Mem_out.hit <= '1' when (Tag_Mem_out.DataO = DCache_Mem_in.Tag_DataI) else '0';  -- Tag comparator
end DCache_Mem_behave;




