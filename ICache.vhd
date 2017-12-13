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
use ieee.math_real.all;
library work;
use work.Constants.all;
use work.icache_pkg.all;

entity ICache is
  generic(IC_Block_Size      : natural := IC_Block_Size;
          Address_Space_CPU  : natural := Address_Space_CPU;
          Address_Space_IMem : natural := Address_Space_IMem;
          Instr_Size         : natural := Instr_Size;
          IC_Offset_in_bits  : natural := IC_Offset_in_bits;
          IC_Tag_Size        : natural := IC_Tag_Size;
          IC_Index_in_Bits   : natural := IC_Index_in_Bits;
          IC_Num_Data_Mem    : natural := IC_Num_Data_Mem;
          IC_Data_Mem_Width  : natural := IC_Data_Mem_Width;
          IMem2DataWRatio    : natural := IMem2DataWRatio;
          Num_IMem_Refer     : natural := Num_IMem_Refer;
          IC_LRU_Table_Width : natural := IC_LRU_Table_Width;
          IC_Num_Of_Ways     : natural := IC_Num_Of_Ways;
          IC_Num_Of_Sets     : natural := IC_Num_Of_Sets;
          IC_Rep_Policy      : natural := IC_Rep_Policy;
          LRU                : natural := LRU);
  port(clk        : in  std_logic;
       Reset      : in  std_logic;
       ICache_in  : in  ICache_in_type;
       ICache_out : out ICache_out_type);
end ICache;

architecture ICache_behave of ICache is
  signal Controller_in        : Controller_in_type;
  signal Controller_out       : Controller_out_type;
  signal LRU_Cal_in           : LRU_Cal_in_type;
  signal LRU_Cal_out          : LRU_Cal_out_type;
  signal LRU_Rep_in           : LRU_Rep_in_type;
  signal LRU_Rep_out          : LRU_Rep_out_type;
  signal PseudoRan_Rep_out    : PseudoRan_Rep_out_type;
  signal LRU_Table_in         : LRU_Table_in_type;
  signal LRU_Table_out        : LRU_Table_out_type;
  signal LRU_Table_zero       : std_logic_vector(IC_LRU_Table_Width-1 downto 0);
  type   ICache_Mem_in_array_type is array (IC_Num_Of_Ways-1 downto 0) of ICache_Mem_in_type;
  signal ICache_Mem_in_array  : ICache_Mem_in_array_type;
  type   ICache_Mem_out_array_type is array (IC_Num_Of_Ways-1 downto 0) of ICache_Mem_out_type;
  signal ICache_Mem_out_array : ICache_Mem_out_array_type;
  signal One_Way_Data         : std_logic_vector (IC_Block_Size-1 downto 0);
  signal Tag_plus_Vbit        : std_logic_vector(IC_Tag_Size downto 0);
  signal tag_data_last        : std_logic_vector(IC_Tag_Size downto 0);
  signal tag_data_zero        : std_logic_vector(IC_Tag_Size downto 0);
  signal offset_integer       : natural range 0 to Instr_in_Block-1;
  signal tag_addr             : std_logic_vector (IC_Index_in_Bits-1 downto 0);
  signal tag_addr_last        : std_logic_vector (IC_Index_in_Bits-1 downto 0);
  signal First_Addr_Block     : natural;
  signal en_Way_Wr            : std_logic_vector(IC_Num_Of_Ways-1 downto 0);
  signal LRU_Wr_addr          : std_logic_vector(IC_Index_in_Bits-1 downto 0);
  signal hit                  : std_logic;
  signal PC                   : std_logic_vector (Address_Space_CPU-1 downto 0);
  
begin

    process(clk)
    begin
      if rising_edge(clk) then
        if Reset = '0' then
          PC <= (others => '0');
        elsif ICache_in.IFetch_Stopped = '0' then
          PC <= ICache_in.PC;
        end if;
      end if;
    end process;

  IC_LRU_gen : if(IC_Rep_Policy = LRU and IC_Num_Of_Ways /= 1) generate
    LRU_Cal_in.LRU_Cur <= LRU_Table_out.DataO;
    MyIC_LRU_Cal : IC_LRU_Cal
      generic map(IC_Num_Of_Ways)
      port map(LRU_Cal_in  => LRU_Cal_in,
               LRU_Cal_out => LRU_Cal_out);

    LRU_Rep_in.LRU_Cur <= LRU_Table_out.DataO;
    MyIC_LRU_Rep : IC_LRU_Rep
      generic map(IC_Num_Of_Ways)
      port map(LRU_Rep_in  => LRU_Rep_in,
               LRU_Rep_out => LRU_Rep_out);

    LRU_Table_in.en_LRU    <= (Controller_out.en_flush or (Controller_out.en_cache_Tag and not(Controller_out.write_cache_Tag)));
    LRU_Table_in.Read_Addr <= tag_addr;
    LRU_Table_in.Wr_LRU    <= (Controller_out.en_flush or Controller_in.hit);

    MyIC_LRU_Table : IC_LRU_Table
      generic map(IC_Num_Of_Sets,
                  IC_LRU_Table_Width)
      port map(clk           => clk,
               LRU_Table_in  => LRU_Table_in,
               LRU_Table_out => LRU_Table_out);

    LRU_Table_zero <= (others => '0');

    process(clk)
    begin
      if rising_edge(clk) then
        LRU_Wr_addr <= LRU_Table_in.Read_Addr;
      end if;
    end process;

    --MUX to select write address of LRU Table
    LRU_Table_in.Wr_Addr <= LRU_Wr_addr        when (Controller_out.en_flush = '0') else std_logic_vector(to_unsigned(Controller_out.counter_flush, IC_Index_in_Bits));
    --MUX to select write data of LRU Table
    LRU_Table_in.DataI   <= LRU_Cal_out.LRU_Ne when (Controller_out.en_flush = '0') else LRU_Table_zero;

    process(LRU_Rep_out.en_Way, Controller_out.en_flush)
    begin
      for i in 0 to IC_Num_Of_Ways-1 loop
        en_Way_Wr(i) <= (LRU_Rep_out.en_Way(i) or Controller_out.en_flush);
      end loop;
    end process;
  end generate;

  PseudoRan_gen : if (IC_Rep_Policy = PseudoRan and IC_Num_Of_Ways /= 1) generate
    MyIC_PseudoRan_Rep : IC_PseudoRan_Rep
      generic map(IC_Num_Of_Ways,
                  LFSR_Width)       
      port map(Clk               => clk,
               Reset             => Reset,
               PseudoRan_Rep_out => PseudoRan_Rep_out);
    process(clk)
    begin
      if(rising_edge(clk))then
        if(Controller_out.en_flush = '1' or (Controller_out.en_cache_Tag = '1' and Controller_out.write_cache_Tag = '0')) then
          for i in 0 to IC_Num_Of_Ways-1 loop
            en_Way_Wr(i) <= (PseudoRan_Rep_out.en_Way(i) or Controller_out.en_flush);
          end loop;
        end if;
      end if;
    end process;
  end generate;

  Dir_Map_gen : if(IC_Num_Of_Ways = 1) generate  -- direct map
    en_Way_Wr(0) <= '1';
  end generate;

  ICache_Mem_Inst : for i in 0 to IC_Num_Of_Ways-1 generate
  begin

    Input_ICache_Mem_Init : for j in 0 to IC_Num_Data_Mem-1 generate
      ICache_Mem_in_array(i).write_cache_Data(j) <= Controller_out.write_cache_Data(j) and en_Way_Wr(i);
    end generate;
    ICache_Mem_in_array(i).en_cache_Data   <= Controller_out.en_cache_Data;
    ICache_Mem_in_array(i).write_cache_Tag <= Controller_out.write_cache_Tag and en_Way_Wr(i);
    ICache_Mem_in_array(i).en_cache_Tag    <= Controller_out.en_cache_Tag;
    ICache_Mem_in_array(i).IMem_Data       <= ICache_in.IMem_Data;
    ICache_Mem_in_array(i).Set_Addr        <= tag_addr_last;
    ICache_Mem_in_array(i).Tag_DataI       <= tag_data_last;

    MyICache_Mem : ICache_Mem
      generic map(IC_Tag_Size,
                  IC_Index_in_Bits,
                  IC_Num_Data_Mem,
                  IC_Data_Mem_Width,
                  IMem2DataWRatio)         
      port map(clk            => clk,
               Reset          => Reset,  -- active low
               ICache_Mem_in  => ICache_Mem_in_array(i),
               ICache_Mem_out => ICache_Mem_out_array(i));

    LRU_Cal_in.hit(i) <= ICache_Mem_out_array(i).hit;
  end generate;

  Controller_in.ready_IMem <= ICache_in.ready_IMem;
  Controller_in.Extr_Stall <= ICache_in.Extr_Stall;

  MyIC_Controller : IC_Controller
    generic map(Num_IMem_Refer,
                IMem2DataWRatio)
    port map(clk            => clk,
             Reset          => Reset,   -- active low
             Controller_in  => Controller_in,
             Controller_out => Controller_out);  

  First_Addr_Block        <= to_integer(unsigned(PC(Address_Space_CPU-1 downto IC_Offset_in_bits)))* Num_IMem_Refer*4;
  ICache_out.Address2IMem <= std_logic_vector(to_unsigned((First_Addr_Block+(Controller_out.counter_Mem*4)), address_space_IMem));
  Tag_plus_Vbit           <= '1' & PC(address_space_CPU-1 downto IC_Index_in_Bits+IC_Offset_in_bits);
  tag_data_zero           <= (others => '0');
  offset_integer <= to_integer(unsigned(PC(IC_Offset_in_bits-1 downto 0)));
  -- Mux to choose ICache address
  tag_addr <= ICache_in.PC(IC_Index_in_Bits+IC_Offset_in_bits-1 downto IC_Offset_in_bits) when (ICache_in.IFetch_Stopped = '0')
              else PC(IC_Index_in_Bits+IC_Offset_in_bits-1 downto IC_Offset_in_bits);
  --MUX to select write address of Tag memory
  tag_addr_last <= tag_addr when (Controller_out.en_flush = '0') else std_logic_vector(to_unsigned(Controller_out.counter_flush, IC_Index_in_Bits));
  --MUX to select write data of Tag memory
  tag_data_last <= Tag_plus_Vbit when (Controller_out.en_flush = '0') else tag_data_zero;
  process(ICache_Mem_out_array)
  begin
    hit          <= '0';
    One_Way_Data <= (others => '0');
    for i in 0 to IC_Num_Of_Ways-1 loop
      if(ICache_Mem_out_array(i).hit = '1') then
        hit          <= '1';
        One_Way_Data <= ICache_Mem_out_array(i).ICache_DataO_Block;
        exit;
      end if;
    end loop;
  end process;

  Controller_in.hit <= hit when (Controller_out.en_flush = '0') else '0';
  ICache_out.hit                <= Controller_in.hit;
  ICache_out.ICache_DataO_Instr <= One_Way_Data(Instr_Size*(Offset_integer+1)-1 downto Instr_Size*Offset_integer);
  ICache_out.Re_IMem            <= Controller_out.Re_IMem;
  ICache_out.Mem_Acc_Finished   <= Controller_out.Mem_Acc_Finished;
end ICache_behave;





