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
use work.dcache_pkg.all;

entity DC_Controller_WrThrNoAlloc is
  generic(Num_DMem_Refer   : natural := Num_DMem_Refer;
          DMem2DataWRatio  : natural := DMem2DataWRatio;
          DC_Num_Of_Sets   : natural := DC_Num_Of_Sets;
          DC_Bytes_in_Word : natural := DC_Bytes_in_Word);
  port (clk            : in  std_logic;
        Reset          : in  std_logic;  -- active low
        Controller_in  : in  Controller_in_type;
        Controller_out : out Controller_out_type);
end DC_Controller_WrThrNoAlloc;

architecture DC_Controller_behave_WrThrNoAlloc of DC_Controller_WrThrNoAlloc is
  type   TYPE_STATE is (Flush, Idle, Comp_Tag, Upda_Cache, Wr_Mem, Read_Mem);
  signal CURRENT_STATE    : TYPE_STATE;
  signal NEXT_STATE       : TYPE_STATE;
  signal counter_tmp      : natural;    -- range 0 to Num_of_Sets+1;
  signal rst_counter      : std_logic;
  signal en_counter_flush : std_logic;
  signal en_counter_Mem   : std_logic;  
begin
  combinational : process(Controller_in.Extr_Stall, CURRENT_STATE, Controller_in.Block_Offset_int, Controller_in.hit, Controller_in.ready_DMem, counter_tmp, Controller_in.CPU_Wr, Controller_in.CPU_Re, Controller_in.CPU_Re_comb, Controller_in.CPU_Wr_comb, Controller_in.Mask)
  begin
    Controller_out.en_cache_Data    <= (others => '0');
    Controller_out.write_cache_Data <= (others => '0');
    Controller_out.en_cache_Tag     <= '0';
    Controller_out.write_cache_Tag  <= '0';
    Controller_out.Re_DMem          <= '0';
    Controller_out.Wr_DMem          <= '0';
    Controller_out.en_flush         <= '0';
    Controller_out.ready_DCache     <= '1';
    rst_counter                     <= '1';  --active low
    en_counter_Mem                  <= '0';
    en_counter_flush                <= '0';
    Controller_out.counter_Mem_Re   <= 0;
    Controller_out.counter_flush    <= 0;
    Controller_out.Mem_Acc_Finished <= '1';
    NEXT_STATE <= Idle;

    case CURRENT_STATE is
      when Flush =>
        if(counter_tmp < DC_Num_Of_Sets) then
          Controller_out.counter_flush   <= counter_tmp;
          Controller_out.en_cache_Tag    <= '1';
          Controller_out.write_cache_Tag <= '1';
          Controller_out.en_flush        <= '1';
          en_counter_flush               <= '1';
          Controller_out.ready_DCache    <= '0';
          NEXT_STATE                     <= Flush;
        elsif(counter_tmp = DC_Num_Of_Sets) then
          rst_counter <= '0';
          NEXT_STATE  <= Idle;
        end if;
      when Idle =>
        if((Controller_in.CPU_Re_comb = '0' and Controller_in.CPU_Wr_comb = '0') or (Controller_in.Extr_Stall = '1')) then
          NEXT_STATE <= Idle;
        else
          Controller_out.en_cache_Tag  <= '1';
          Controller_out.en_cache_Data <= (others => '1');
          NEXT_STATE                   <= Comp_Tag;
        end if;
      when Comp_Tag =>
        if(Controller_in.hit = '1' and Controller_in.CPU_Re = '1') then
          if((Controller_in.CPU_Re_Comb = '1' or Controller_in.CPU_Wr_Comb = '1') and Controller_in.Extr_Stall = '0') then
            Controller_out.en_cache_Tag  <= '1';
            Controller_out.en_cache_Data <= (others => '1');
            NEXT_STATE                   <= Comp_Tag;
          else
            NEXT_STATE <= Idle;
          end if;
        elsif(Controller_in.hit = '0' and Controller_in.CPU_Re = '1') then
          en_counter_Mem                  <= '1';
          Controller_out.ready_DCache     <= '0';
          Controller_out.Re_DMem          <= '1';
          Controller_out.Mem_Acc_Finished <= '0';
          NEXT_STATE                      <= Read_Mem;
        elsif(Controller_in.hit = '0' and Controller_in.CPU_Wr = '1') then
          Controller_out.ready_DCache     <= '0';
          Controller_out.Wr_DMem          <= '1';
          Controller_out.Mem_Acc_Finished <= '0';
          NEXT_STATE                      <= Wr_Mem;
        elsif(Controller_in.hit = '1' and Controller_in.CPU_Wr = '1') then
          Controller_out.ready_DCache <= '0';
          for i in 0 to DC_Bytes_in_Word-1 loop
            if(Controller_in.Mask(i) = '1') then
              Controller_out.en_cache_Data((Controller_in.Block_Offset_int*DC_Bytes_in_Word)+i)     <= '1';
              Controller_out.write_cache_Data ((Controller_in.Block_Offset_int*DC_Bytes_in_Word)+i) <= '1';
            end if;
          end loop;
          NEXT_STATE <= Upda_Cache;
        end if;
      when Read_Mem =>
        if(counter_tmp = Num_DMem_Refer) then
          rst_counter <= '0';
          if((Controller_in.CPU_Re_Comb = '1' or Controller_in.CPU_Wr_Comb = '1') and Controller_in.Extr_Stall = '0') then
            Controller_out.en_cache_Tag  <= '1';
            Controller_out.en_cache_Data <= (others => '1');
            NEXT_STATE                   <= Comp_Tag;
          else
            NEXT_STATE <= Idle;
          end if;
        elsif(counter_tmp = Num_DMem_Refer-1) then
          Controller_out.ready_DCache <= '0';
          en_counter_Mem              <= '1';
          NEXT_STATE                  <= Read_Mem;
          if(Controller_in.ready_DMem = '1') then
            Controller_out.en_cache_Data                                                                          <= (others => '1');
            Controller_out.write_cache_Data((counter_tmp+1)*DMem2DataWRatio-1 downto counter_tmp*DMem2DataWRatio) <= (others => '1');
            Controller_out.en_cache_Tag                                                                           <= '1';
            Controller_out.write_cache_Tag                                                                        <= '1';
          else
            Controller_out.Mem_Acc_Finished <= '0';
            Controller_out.counter_Mem_Re   <= counter_tmp;
            Controller_out.Re_DMem          <= '1';
          end if;
        elsif(counter_tmp < Num_DMem_Refer-1) then
          Controller_out.ready_DCache     <= '0';
          en_counter_Mem                  <= '1';
          Controller_out.Mem_Acc_Finished <= '0';
          Controller_out.Re_DMem          <= '1';
          Controller_out.counter_Mem_Re   <= counter_tmp;
          if(Controller_in.ready_DMem = '1') then
            Controller_out.counter_Mem_Re                                                                         <= counter_tmp+1;
            Controller_out.en_cache_Data((counter_tmp+1)*DMem2DataWRatio-1 downto counter_tmp*DMem2DataWRatio)    <= (others => '1');
            Controller_out.write_cache_Data((counter_tmp+1)*DMem2DataWRatio-1 downto counter_tmp*DMem2DataWRatio) <= (others => '1');
          end if;
          NEXT_STATE <= Read_Mem;
        end if;
      when Wr_Mem =>
        if(Controller_in.ready_DMem = '1')then
          if((Controller_in.CPU_Re_Comb = '1' or Controller_in.CPU_Wr_Comb = '1') and Controller_in.Extr_Stall = '0') then
            Controller_out.en_cache_Tag  <= '1';
            Controller_out.en_cache_Data <= (others => '1');
            NEXT_STATE                   <= Comp_Tag;
          else
            NEXT_STATE <= Idle;
          end if;
        else
          Controller_out.Mem_Acc_Finished <= '0';
          Controller_out.ready_DCache     <= '0';
          Controller_out.Wr_DMem          <= '1';
          NEXT_STATE                      <= Wr_Mem;
        end if;
      when Upda_Cache =>
        Controller_out.ready_DCache     <= '0';
        Controller_out.Mem_Acc_Finished <= '0';
        Controller_out.Wr_DMem          <= '1';
        NEXT_STATE                      <= Wr_Mem;       
      when others =>
        rst_counter                     <= '1';
        en_counter_flush                <= '0';
        en_counter_Mem                  <= '0';
        Controller_out.en_cache_Data    <= (others => '0');
        Controller_out.write_cache_Data <= (others => '0');
        Controller_out.en_cache_Tag     <= '0';
        Controller_out.write_cache_Tag  <= '0';
        Controller_out.Re_DMem          <= '0';
        Controller_out.Wr_DMem          <= '0';
        Controller_out.ready_DCache     <= '1';
        Controller_out.Mem_Acc_Finished <= '1';
        NEXT_STATE                      <= Flush;
    end case;
  end process;

  sequential : process(clk)
  begin
    if (rising_edge(clk)) then
      if(Reset = '0')then
        CURRENT_STATE <= Flush;
        counter_tmp   <= 0;
      else
        CURRENT_STATE <= NEXT_STATE;
        if(rst_counter = '0') then
          counter_tmp <= 0;
        elsif((Controller_in.ready_DMem = '1' and en_counter_Mem = '1') or en_counter_flush = '1') then
          counter_tmp <= counter_tmp+1;
        end if;
      end if;
    end if;
  end process;
end DC_Controller_behave_WrThrNoAlloc;






