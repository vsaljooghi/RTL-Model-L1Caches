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
use work.icache_pkg.all;
use work.common_pkg.all;

entity IC_PseudoRan_Rep is
  generic(IC_Num_Of_Ways : natural := IC_Num_Of_Ways;
          LFSR_Width     : natural := LFSR_Width);       
  port (Clk               : in  std_logic;
        Reset             : in  std_logic;
        PseudoRan_Rep_out : out PseudoRan_Rep_out_type);
end IC_PseudoRan_Rep;

architecture IC_PseudoRan_Rep_behave of IC_PseudoRan_Rep is
  signal rand_out     : std_logic_vector(LFSR_width-1 downto 0);
  signal rand_out_int : natural range 0 to 2**LFSR_width-1;
  signal en_Way_int   : natural range 0 to IC_Num_Of_Ways-1;

begin
  MyLFSR : lfsr 
    generic map (width => LFSR_Width)
    port map(Clk      => Clk,
             Reset    => Reset,
             rand_out => rand_out);
  rand_out_int <= to_integer(unsigned(rand_out));
  en_Way_int   <= rand_out_int mod IC_Num_Of_Ways;
  en_Way_gen : for i in 0 to IC_Num_Of_Ways-1 generate
    PseudoRan_Rep_out.en_Way(i) <= '1' when (i = en_Way_int) else '0';
  end generate;
end;
