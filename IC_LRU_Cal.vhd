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
library work;
use work.Constants.all;
use work.icache_pkg.all;
use work.common_pkg.all;

entity IC_LRU_Cal is
  generic(IC_Num_Of_Ways : natural := IC_Num_Of_Ways);
  port (LRU_Cal_in  : in  LRU_Cal_in_type;
        LRU_Cal_out : out LRU_Cal_out_type);
end IC_LRU_Cal;

architecture IC_LRU_Cal_behave of IC_LRU_Cal is
  signal hit_way_int : integer := 0;
begin
  process(LRU_Cal_in.hit)
  begin
    hit_way_int <= 0;
    for i in 0 to IC_Num_Of_Ways-1 loop
      if(LRU_Cal_in.hit(i) = '1') then
        hit_way_int <= i;
        exit;
      end if;
    end loop;
  end process;
  associ_2way_cal : if (IC_Num_Of_Ways = 2) generate
    LRU_Cal_out.LRU_Ne(0) <= lru_2way_table(conv_integer(LRU_Cal_in.LRU_Cur))(hit_way_int);
  end generate;
  associ_3way_cal : if (IC_Num_Of_Ways = 3) generate
    LRU_Cal_out.LRU_Ne(2 downto 0) <= lru_3way_table(conv_integer(LRU_Cal_in.LRU_Cur))(hit_way_int);
  end generate;
  associ_4way_cal : if (IC_Num_Of_Ways = 4) generate
    LRU_Cal_out.LRU_Ne(4 downto 0) <= lru_4way_table(conv_integer(LRU_Cal_in.LRU_Cur))(hit_way_int);
  end generate;
end IC_LRU_Cal_behave;

