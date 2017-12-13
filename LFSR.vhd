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
  
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity lfsr is
  generic (width : integer := 4);
  port (Clk      : in  std_logic;
        Reset    : in  std_logic;
        rand_out : out std_logic_vector(width-1 downto 0));
end lfsr;

architecture rtl of lfsr is
begin
  process(Clk)
    variable random : std_logic_vector(width-1 downto 0);
  begin
    if(rising_edge(Clk)) then
      if Reset = '1' then
        random := conv_std_logic_vector(1, width);
      end if;
      random (width downto 1) := random(width-1 downto 0);
      case width is
        when 0 => null;
        when 1 => null;
        when 2 => null;
        when 3 =>
          random(0) := random(2) xor random(1);
        when 4 =>
          random(0) := random(3) xor random(2);
        when 5 =>
          random(0) := random(4) xor random(2);
        when 6 =>
          random(0) := random(5) xor random(4);
        when 7 =>
          random(0) := random(6) xor random(5);
        when 8 =>
          random(0) := random(7) xor random(5) xor random(4) xor random(3);
        when 9 =>
          random(0) := random(8) xor random(4);
        when 10 =>
          random(0) := random(9) xor random(6);
        when 11 =>
          random(0) := random(10) xor random(8);
        when 12 =>
          random(0) := random(11) xor random(10) xor random(9) xor random(3);
        when 13 =>
          random(0) := random(12) xor random(11) xor random(10) xor random(7);
        when 14 =>
          random(0) := random(13) xor random(12) xor random(11) xor random(1);
        when 15 =>
          random(0) := random(14) xor random(13);
        when 16 =>
          random(0) := random(15) xor random(13) xor random(12) xor random(10);
        when 17 =>
          random(0) := random(16) xor random(13);
        when 18 =>
          random(0) := random(17) xor random(10);
        when 19 =>
          random(0) := random(18) xor random(17) xor random(15) xor random(13);
        when others =>
          random(0) := random(width-1) xor random(width-2);
      end case;
    end if;
    rand_out <= random;
  end process;
end rtl;


