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

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
library work;
use work.Constants.all;
use work.dcache_pkg.all;
use work.common_pkg.all;

entity DC_LRU_Rep is
  generic(DC_Num_Of_Ways : natural := DC_Num_Of_Ways);
  port (LRU_Rep_in   : in  LRU_Rep_in_type;
         LRU_Rep_out : out LRU_Rep_out_type);
end DC_LRU_Rep;

architecture DC_LRU_Rep_behave of DC_LRU_Rep is
begin
  associ_2way_rep : if (DC_Num_Of_Ways = 2) generate
    LRU_Rep_out.en_Way(DC_Num_Of_Ways-1 downto 0) <= lru2_Rep_table(conv_integer(LRU_Rep_in.LRU_Cur));
  end generate;
  associ_3way_rep : if (DC_Num_Of_Ways = 3) generate
    LRU_Rep_out.en_Way(DC_Num_Of_Ways-1 downto 0) <= lru3_Rep_table(conv_integer(LRU_Rep_in.LRU_Cur));
  end generate;
  associ_4way_rep : if (DC_Num_Of_Ways = 4) generate
    LRU_Rep_out.en_Way(DC_Num_Of_Ways-1 downto 0) <= lru4_Rep_table(conv_integer(LRU_Rep_in.LRU_Cur));
  end generate;
end DC_LRU_Rep_behave;

