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

package common_pkg is

  -- 2-way set permutations
--  10  (way numbers)  
-- w01 => way 1 - least recently used   
--         way 0 - most recently used (greater number means most recently used)
  constant w01 : std_logic := '0';
  constant w10 : std_logic := '1';
  type lru_2way_array_type is array(1 downto 0) of std_logic;
  type lru_2way_table_type is array (0 to 1) of lru_2way_array_type;
  constant lru_2way_table : lru_2way_table_type :=
    ((w10, w01),                        -- w01
     (w10, w01)                         -- w10
     );
  type lru2_Rep_table_type is array(0 to 1) of std_logic_vector (1 downto 0);
  constant lru2_Rep_table : lru2_Rep_table_type :=
    ("10",                              -- w01
     "01"                               -- w10
     ); 

-- 3-way set permutations
--  210  (way numbers)  
-- w201 => way 1 - least recently used   
--         way 2 - most recently used (greater number means most recently used)
  constant w012 : std_logic_vector(2 downto 0) := "000";
  constant w021 : std_logic_vector(2 downto 0) := "001";
  constant w102 : std_logic_vector(2 downto 0) := "010";
  constant w120 : std_logic_vector(2 downto 0) := "011";
  constant w201 : std_logic_vector(2 downto 0) := "100";
  constant w210 : std_logic_vector(2 downto 0) := "101";
  type lru_3way_array_type is array(2 downto 0) of std_logic_vector(2 downto 0);
  type lru_3way_table_type is array (0 to 7) of lru_3way_array_type;
  constant lru_3way_table : lru_3way_table_type :=
    ((w201, w021, w012),                -- w012
     (w210, w021, w012),                -- w021
     (w201, w021, w102),                -- w102
     (w210, w120, w012),                -- w120
     (w201, w120, w102),                -- w201
     (w210, w120, w102),                -- w210
     (w210, w201, w102),  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     (w210, w201, w102)  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     );
  type lru3_Rep_table_type is array(0 to 7) of std_logic_vector (2 downto 0);
  constant lru3_Rep_table : lru3_Rep_table_type :=
    ("100",                             -- w012
     "100",                             -- w021
     "010",                             -- w102
     "001",                             -- w120
     "010",                             -- w201
     "001",                             -- w210
     "001",  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     "001"  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     );
  
-- 4-way set permutations
--  3210  (way numbers) 
-- w0123 => way 3 - least recently used
--          way 0 - most recently used (greater number means most recently used)
  constant w0123 : std_logic_vector(4 downto 0) := "00000";
  constant w0132 : std_logic_vector(4 downto 0) := "00001";
  constant w0213 : std_logic_vector(4 downto 0) := "00010";
  constant w0231 : std_logic_vector(4 downto 0) := "00011";
  constant w0312 : std_logic_vector(4 downto 0) := "00100";
  constant w0321 : std_logic_vector(4 downto 0) := "00101";
  constant w1023 : std_logic_vector(4 downto 0) := "00110";
  constant w1032 : std_logic_vector(4 downto 0) := "00111";
  constant w1203 : std_logic_vector(4 downto 0) := "01000";
  constant w1230 : std_logic_vector(4 downto 0) := "01001";
  constant w1302 : std_logic_vector(4 downto 0) := "01010";
  constant w1320 : std_logic_vector(4 downto 0) := "01011";
  constant w2013 : std_logic_vector(4 downto 0) := "01100";
  constant w2031 : std_logic_vector(4 downto 0) := "01101";
  constant w2103 : std_logic_vector(4 downto 0) := "01110";
  constant w2130 : std_logic_vector(4 downto 0) := "01111";
  constant w2301 : std_logic_vector(4 downto 0) := "10000";
  constant w2310 : std_logic_vector(4 downto 0) := "10001";
  constant w3012 : std_logic_vector(4 downto 0) := "10010";
  constant w3021 : std_logic_vector(4 downto 0) := "10011";
  constant w3102 : std_logic_vector(4 downto 0) := "10100";
  constant w3120 : std_logic_vector(4 downto 0) := "10101";
  constant w3201 : std_logic_vector(4 downto 0) := "10110";
  constant w3210 : std_logic_vector(4 downto 0) := "10111";
  type lru_4way_array_type is array(3 downto 0) of std_logic_vector(4 downto 0);
  type lru_4way_table_type is array(0 to 31) of lru_4way_array_type;
  constant lru_4way_table : lru_4way_table_type :=
    ((w3012, w0312, w0132, w0123),      -- w0123
     (w3021, w0321, w0132, w0123),      -- w0132
     (w3102, w0312, w0132, w0213),      -- w0213
     (w3120, w0321, w0231, w0123),      -- w0231
     (w3201, w0312, w0231, w0213),      -- w0312    
     (w3210, w0321, w0231, w0213),      -- w0321
     (w3012, w0312, w1032, w1023),      -- w1023
     (w3021, w0321, w1032, w1023),      -- w1032
     (w3102, w1302, w0132, w1203),      -- w1203
     (w3120, w1320, w1230, w0123),      -- w1230
     (w3201, w1302, w0231, w1203),      -- w1302
     (w3210, w1320, w1230, w0213),      -- w1320
     (w3012, w1302, w1032, w2013),      -- w2013
     (w3021, w1320, w2031, w1023),      -- w2031
     (w3102, w1302, w1032, w2103),      -- w2103
     (w3120, w1320, w2130, w1023),      -- w2130      
     (w3201, w2301, w1230, w1203),      -- w2301
     (w3210, w2310, w1230, w1203),      -- w2310
     (w3012, w2301, w2031, w2013),      -- w3012
     (w3021, w2310, w2031, w2013),      -- w3021
     (w3102, w2301, w2031, w2103),      -- w3102
     (w3120, w2310, w2130, w2013),      -- w3120
     (w3201, w2301, w2130, w2103),      -- w3201
     (w3210, w2310, w2130, w2103),      -- w3210
     (w3210, w3201, w3102, w2103),  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     (w3210, w3201, w3102, w2103),  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     (w3210, w3201, w3102, w2103),  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     (w3210, w3201, w3102, w2103),  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     (w3210, w3201, w3102, w2103),  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     (w3210, w3201, w3102, w2103),  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     (w3210, w3201, w3102, w2103),  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     (w3210, w3201, w3102, w2103)  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     );
  type     lru4_Rep_table_type is array(0 to 31) of std_logic_vector (3 downto 0);
  constant lru4_Rep_table : lru4_Rep_table_type :=
    ("1000",                            -- w0123
     "1000",                            -- w0132
     "1000",                            -- w0213
     "1000",                            -- w0231
     "1000",                            -- w0312
     "1000",                            -- w0321
     "0100",                            -- w1023
     "0100",                            -- w1032
     "0010",                            -- w1203
     "0001",                            -- w1230
     "0010",                            -- w1302
     "0001",                            -- w1320
     "0100",                            -- w2013
     "0100",                            -- w2031
     "0010",                            -- w2103
     "0001",                            -- w2130
     "0010",                            -- w2301
     "0001",                            -- w2310
     "0100",                            -- w3012
     "0100",                            -- w3021
     "0010",                            -- w3102
     "0001",                            -- w3120
     "0010",                            -- w3201
     "0001",                            -- w3210
     "0001",  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     "0001",  -- It will not be used but since it's possible for LRU table not be initialized it will be useful      
     "0001",  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     "0001",  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     "0001",  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     "0001",  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     "0001",  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     "0001"  -- It will not be used but since it's possible for LRU table not be initialized it will be useful
     );

  function factorial(n : integer) return integer;

  component lfsr is
    generic (width : integer := 4);
    port (Clk      : in  std_logic;
          Reset    : in  std_logic;
          rand_out : out std_logic_vector(width-1 downto 0));
  end component lfsr;

end package;

package body common_pkg is
  function factorial(n : integer) return integer is
    variable n_fact : integer;
    begin
      n_fact := 1;
      for i in 1 to n loop
        n_fact := n_fact*i;
      end loop;
      return n_fact;
  end factorial;
end package body;
