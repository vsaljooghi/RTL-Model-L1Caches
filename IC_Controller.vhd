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
use ieee.numeric_std.all;
use work.Constants.all;
use work.icache_pkg.all;

entity IC_Controller is
  generic(Num_IMem_Refer  : natural := Num_IMem_Refer;
          IMem2DataWRatio : natural := IMem2DataWRatio);
  port (clk            : in  std_logic;
        Reset          : in  std_logic;  -- active low
        Controller_in  : in  Controller_in_type;
        Controller_out : out Controller_out_type);
end IC_Controller;

architecture IC_Controller_behave of IC_Controller is
  type   TYPE_STATE is (Flush, Comp_Tag, Read_Mem);
  signal CURRENT_STATE    : TYPE_STATE;
  signal NEXT_STATE       : TYPE_STATE;
  signal counter_tmp      : natural;
  signal rst_counter      : std_logic := '0';
  signal en_counter_flush : std_logic;
  signal FirstTime        : std_logic;
  signal en_counter_Mem   : std_logic; 
begin
  combinational : process(Controller_in.Extr_Stall, FirstTime, CURRENT_STATE, Controller_in.hit, Controller_in.ready_IMem, counter_tmp)
  begin
    Controller_out.en_cache_Data    <= (others => '0');
    Controller_out.write_cache_Data <= (others => '0');
    Controller_out.en_cache_Tag     <= '0';
    Controller_out.write_cache_Tag  <= '0';
    Controller_out.Re_IMem          <= '0';
    Controller_out.en_flush         <= '0';
    rst_counter                     <= '1';
    en_counter_Mem                  <= '0';
    en_counter_flush                <= '0';
    Controller_out.counter_flush    <= 0;
    Controller_out.counter_Mem      <= 0;
    Controller_out.Mem_Acc_Finished <= '1';
    NEXT_STATE <= Comp_Tag;

    case CURRENT_STATE is
      when Flush =>
        if(counter_tmp < IC_Num_Of_Sets) then
          Controller_out.counter_flush   <= counter_tmp;
          Controller_out.en_cache_Tag    <= '1';
          Controller_out.write_cache_Tag <= '1';
          Controller_out.en_flush        <= '1';
          en_counter_flush               <= '1';
          NEXT_STATE                     <= Flush;
        elsif(counter_tmp = IC_Num_Of_Sets) then
          rst_counter                  <= '0';
          Controller_out.en_cache_Tag  <= '1';
          Controller_out.en_cache_Data <= (others => '1');
          NEXT_STATE                   <= Comp_Tag;
        end if;
      when Comp_Tag =>
        if(FirstTime = '0' and Controller_in.Extr_Stall = '1') then
          NEXT_STATE <= Comp_Tag;
        elsif(Controller_in.hit = '1' and (Controller_in.Extr_Stall = '0' or FirstTime = '1')) then
          Controller_out.en_cache_Data <= (others => '1');
          Controller_out.en_cache_Tag  <= '1';
          NEXT_STATE                   <= Comp_Tag;
        else
          en_counter_Mem                  <= '1';
          Controller_out.Re_IMem          <= '1';
          Controller_out.Mem_Acc_Finished <= '0';
          NEXT_STATE                      <= Read_Mem;
        end if;       
      when Read_Mem =>
        if(counter_tmp = Num_IMem_Refer-1) then
          if(Controller_in.ready_IMem = '1') then
            rst_counter                                                                                           <= '0';
            Controller_out.en_cache_Data                                                                          <= (others => '1');
            Controller_out.write_cache_Data((counter_tmp+1)*IMem2DataWRatio-1 downto counter_tmp*IMem2DataWRatio) <= (others => '1');
            Controller_out.en_cache_Tag                                                                           <= '1';
            Controller_out.write_cache_Tag                                                                        <= '1';
            NEXT_STATE                                                                                            <= Comp_Tag;
          else
            Controller_out.Mem_Acc_Finished <= '0';
            Controller_out.counter_Mem      <= counter_tmp;
            Controller_out.Re_IMem          <= '1';
            NEXT_STATE                      <= Read_Mem;
          end if;        
        elsif(counter_tmp < Num_IMem_Refer-1) then
          en_counter_Mem                  <= '1';
          Controller_out.Mem_Acc_Finished <= '0';
          Controller_out.counter_Mem      <= counter_tmp;
          Controller_out.Re_IMem          <= '1';
          if(Controller_in.ready_IMem = '1') then
            Controller_out.counter_Mem                                                                            <= counter_tmp+1;
            Controller_out.en_cache_Data((counter_tmp+1)*IMem2DataWRatio-1 downto counter_tmp*IMem2DataWRatio)    <= (others => '1');
            Controller_out.write_cache_Data((counter_tmp+1)*IMem2DataWRatio-1 downto counter_tmp*IMem2DataWRatio) <= (others => '1');
          end if;
          NEXT_STATE <= Read_Mem;
        end if;
      when others =>
        rst_counter                     <= '1';
        en_counter_Mem                  <= '0';
        en_counter_flush                <= '0';
        Controller_out.en_cache_Data    <= (others => '0');
        Controller_out.write_cache_Data <= (others => '0');
        Controller_out.en_cache_Tag     <= '0';
        Controller_out.write_cache_Tag  <= '0';
        Controller_out.Re_IMem          <= '0';
        Controller_out.Mem_Acc_Finished <= '1';
        NEXT_STATE                      <= Comp_Tag;
    end case;
  end process;

  sequential : process(clk)
  begin
    if (rising_edge(clk)) then
      if Reset = '0' then
        CURRENT_STATE <= Flush;
        FirstTime     <= '1';
        counter_tmp   <= 0;
      else
        CURRENT_STATE <= NEXT_STATE;
        if(Controller_in.Extr_Stall = '1') then
          FirstTime <= '0';
        else
          FirstTime <= '1';
        end if;
        if(rst_counter = '0') then
          counter_tmp <= 0;
        elsif((Controller_in.ready_IMem = '1' and en_counter_Mem = '1') or en_counter_flush = '1')then
          counter_tmp <= counter_tmp+1;
        end if;
      end if;
    end if;
  end process;
end IC_Controller_behave;



