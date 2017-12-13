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

entity DC_Tag_Mem is
  generic(DC_Index_in_Bits : natural := DC_Index_in_Bits;
          DC_Tag_Size      : natural := DC_Tag_Size);
  port (clk          : in  std_logic;
         Tag_Mem_in  : in  Tag_Mem_in_type;
         Tag_Mem_out : out Tag_Mem_out_type);
end DC_Tag_Mem;

architecture DC_Tag_Mem_behave of DC_Tag_Mem is
  subtype set_index is natural range 0 to 2**DC_Index_in_Bits- 1;
  type    Tag_Mem_Type is array (set_index) of std_logic_vector(DC_Tag_Size downto 0);  -- 1 bit is for valid bit
  signal  Tag_Mem  : Tag_Mem_Type;
  signal  addr_tmp : set_index;
begin
  addr_tmp <= conv_integer(Tag_Mem_in.Address);
  process(clk)
  begin
    if (rising_edge(clk)) then
      if(Tag_Mem_in.Tag_en = '1') then
        if (Tag_Mem_in.Write_tag = '1') then
          Tag_Mem(addr_tmp) <= Tag_Mem_in.DataI;
        end if;
        if(Tag_Mem_in.Write_tag = '1') then  --Read happens at the same clk after writing
          Tag_Mem_out.DataO <= Tag_Mem_in.DataI;
        else
          Tag_Mem_out.DataO <= Tag_Mem(addr_tmp);
        end if;
      end if;
    end if;
  end process;
end DC_Tag_Mem_behave;

