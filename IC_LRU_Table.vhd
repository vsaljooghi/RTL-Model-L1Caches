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

entity IC_LRU_Table is
  generic(IC_Num_Of_Sets     : natural := IC_Num_Of_Sets;
          IC_LRU_Table_Width : natural := IC_LRU_Table_Width);        
  port (clk           : in  std_logic;
        LRU_Table_in  : in  LRU_Table_in_type;
        LRU_Table_out : out LRU_Table_out_type);
end IC_LRU_Table;

architecture IC_LRU_Table_behave of IC_LRU_Table is
  subtype set_index is natural range 0 to IC_Num_Of_Sets- 1;
  type    LRU_Table_Type is array (set_index) of std_logic_vector(IC_LRU_Table_Width-1 downto 0);
  signal  LRU_Table     : LRU_Table_Type;
  signal  Read_addr_tmp : set_index;
  signal  Wr_addr_tmp   : set_index;
begin
  Read_addr_tmp <= conv_integer(LRU_Table_in.Read_Addr);
  Wr_addr_tmp   <= conv_integer(LRU_Table_in.Wr_Addr);
  process(clk)
  begin
    if (rising_edge(clk)) then
      if(LRU_Table_in.en_LRU = '1') then
        if (LRU_Table_in.Wr_LRU = '1') then
          LRU_Table(Wr_addr_tmp) <= LRU_Table_in.DataI;
        end if;
        if(LRU_Table_in.Wr_LRU = '1' and Wr_addr_tmp = Read_addr_tmp) then
          LRU_Table_out.DataO <= LRU_Table_in.DataI;
        else
          LRU_Table_out.DataO <= LRU_Table(Read_addr_tmp);
        end if;
      end if;
    end if;
  end process;
end IC_LRU_Table_behave;






