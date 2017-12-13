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
library work;
use work.cache_pkg.all;
use work.arbiter_pkg.all;

entity arbiter is
  port(
    Clk             : in  std_logic;
    Reset           : in  std_logic;
    arbiter_ic_in   : in  arbiter_ic_in_record;
    arbiter_ic_out  : out arbiter_ic_out_record;
    arbiter_dc_in   : in  arbiter_dc_in_record;
    arbiter_dc_out  : out arbiter_dc_out_record;
    arbiter_mem_in  : in  mem_in_record;
    arbiter_mem_out : out mem_out_record);
end arbiter;

architecture rtl of arbiter is
  type   type_state is (NoMemAcc,DataMemAcc,InstrMemAcc);
  signal current_state : type_state;
  signal next_state    : type_state;
  signal mem_ready     : std_logic;
  signal arbiter_ctrl  : std_logic;
  signal mem_out_wr    : std_logic;
  signal mem_out_rd    : std_logic;
begin
  arbiter_mem_out.rd   <= mem_out_rd;
  arbiter_mem_out.wr   <= mem_out_wr;
  arbiter_mem_out.data <= arbiter_dc_in.data;
  arbiter_mem_out.mask <= arbiter_dc_in.mask;

  process(arbiter_ctrl, arbiter_dc_in, arbiter_ic_in, arbiter_mem_in, mem_ready)
  begin
    if arbiter_ctrl = '1' then
      arbiter_mem_out.addr <= arbiter_dc_in.addr;
      arbiter_dc_out.data  <= arbiter_mem_in.data;
      mem_out_wr           <= arbiter_dc_in.wr;
      arbiter_dc_out.ready <= arbiter_mem_in.stall;
      mem_out_rd           <= arbiter_dc_in.rd;
      arbiter_ic_out.ready <= '0';
      arbiter_ic_out.data  <= (others => '0');
    else
      arbiter_dc_out.data  <= (others => '0');
      arbiter_dc_out.ready <= '0';
      arbiter_mem_out.addr <= arbiter_ic_in.addr;
      arbiter_ic_out.data  <= arbiter_mem_in.data;
      mem_out_wr           <= '0';
      arbiter_ic_out.ready <= arbiter_mem_in.stall;
      mem_out_rd           <= arbiter_ic_in.rd;
    end if;
  end process;

  combinational : process(current_state, arbiter_dc_in, arbiter_ic_in)
  begin
    arbiter_ctrl <= '1';

    case current_state is
      when NoMemAcc =>
        if arbiter_dc_in.rd = '1' or arbiter_dc_in.wr = '1' then
          next_state                    <= DataMemAcc;
        elsif arbiter_ic_in.rd = '1' then
          next_state <= InstrMemAcc;
        else
          next_state <= NoMemAcc;
        end if;
      when DataMemAcc =>
        if arbiter_dc_in.acc = '0' then
          next_state <= DataMemAcc;
        else
          if arbiter_ic_in.rd = '1' then
            next_state <= InstrMemAcc;
          else
            next_state <= NoMemAcc;
          end if;
        end if;
      when InstrMemAcc =>
        arbiter_ctrl <= '0';
        if arbiter_ic_in.acc = '0' then
          next_state <= InstrMemAcc;
        else
          if arbiter_dc_in.rd = '1' or arbiter_dc_in.wr = '1' then
            next_state <= DataMemAcc;
          else
            next_state <= NoMemAcc;
          end if;
        end if;
      when others =>
        next_state <= NoMemAcc;
    end case;
  end process;

  sequential : process(Clk)
  begin
    if (rising_edge(Clk)) then
      if Reset = '0' then
        current_state <= NoMemAcc;
      else
        current_state <= next_state;
      end if;
    end if;
  end process;
end rtl;
