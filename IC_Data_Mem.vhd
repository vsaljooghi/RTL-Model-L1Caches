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
use IEEE.STD_LOGIC_UNSIGNED.all;
library work;
use work.Constants.all;
use work.icache_pkg.all;

entity IC_Data_Mem is
  generic(IC_Index_in_Bits  : natural := IC_Index_in_Bits;
          IC_Data_Mem_Width : natural := IC_Data_Mem_Width);
  port (clk          : in  std_logic;
        Data_Mem_in  : in  Data_Mem_in_type;
        Data_Mem_out : out Data_Mem_out_type);
end IC_Data_Mem;

architecture IC_Data_Mem_behave of IC_Data_Mem is
  subtype set_index is natural range 0 to 2**IC_Index_in_Bits-1;
  type    Data_Mem_Type is array (set_index) of std_logic_vector(IC_Data_Mem_Width-1 downto 0);
  signal  Data_Mem : Data_Mem_Type;
  signal  addr_tmp : set_index;
begin
  addr_tmp <= conv_integer(Data_Mem_in.Address);
  process(clk)
  begin
    if (rising_edge(clk)) then
      if(Data_Mem_in.Data_en = '1') then
        if (Data_Mem_in.Write_data = '1') then
          Data_Mem(addr_tmp) <= Data_Mem_in.DataI;
        end if;
        if(Data_Mem_in.Write_data = '1') then
          Data_Mem_out.DataO <= Data_Mem_in.DataI;
        else
          Data_Mem_out.DataO <= Data_Mem(addr_tmp);
        end if;
      end if;
    end if;
  end process;
end IC_Data_Mem_behave;

