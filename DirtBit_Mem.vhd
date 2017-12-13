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
use work.dcache_pkg.all;

entity DirtBit_Mem is
  generic(DC_Index_in_Bits : natural := DC_Index_in_Bits);
  port (clk              : in  std_logic;
         Reset           : in  std_logic;
         DirtBit_Mem_in  : in  DirtBit_Mem_in_type;
         DirtBit_Mem_out : out DirtBit_Mem_out_type);
end DirtBit_Mem;

architecture DirtBit_Mem_behave of DirtBit_Mem is
  subtype set_index is natural range 0 to 2**DC_Index_in_Bits- 1;
  type    DirtBit_Mem_Type is array (set_index) of std_logic;
  signal  DirtBit_Mem : DirtBit_Mem_Type;
  signal  addr_tmp    : set_index;
begin
  addr_tmp              <= conv_integer(DirtBit_Mem_in.Address);
  DirtBit_Mem_out.DataO <= DirtBit_Mem(addr_tmp);
  process(clk)
  begin
    if (rising_edge(clk)) then
      if(Reset = '0') then
        DirtBit_Mem <= (others => '0');
      elsif (DirtBit_Mem_in.en = '1') then
        DirtBit_Mem(addr_tmp) <= DirtBit_Mem_in.DataI;
      end if;
    end if;
  end process;
end DirtBit_Mem_behave;
