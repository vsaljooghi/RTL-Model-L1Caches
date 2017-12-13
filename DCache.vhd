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
use std.textio.all;
library work;
use work.Constants.all;
use work.dcache_pkg.all;

entity DCache is
  generic(DC_Block_Size           : natural := DC_Block_Size;
          Address_Space_CPU       : natural := Address_Space_CPU;
          Address_Space_DMem      : natural := Address_Space_DMem;
          Word_Size               : natural := Word_Size;
          DMemWidth              : natural := DMemWidth;
          DC_Block_Offset_in_bits : natural := DC_Block_Offset_in_bits;
          DC_Tag_Size             : natural := DC_Tag_Size;
          DC_Index_in_Bits        : natural := DC_Index_in_Bits;
          DC_Num_Data_Mem         : natural := DC_Num_Data_Mem;
          DC_Data_Mem_Width       : natural := DC_Data_Mem_Width;
          DMem2DataWRatio         : natural := DMem2DataWRatio;
          Num_DMem_Refer          : natural := Num_DMem_Refer;
          DC_LRU_Table_Width      : natural := DC_LRU_Table_Width;
          DC_Num_Of_Ways          : natural := DC_Num_Of_Ways;
          DC_Num_Of_Sets          : natural := DC_Num_Of_Sets;
          DC_Rep_Policy           : natural := DC_Rep_Policy;
          LRU                     : natural := LRU);
  port(clk        : in  std_logic;
       Reset      : in  std_logic;
       DCache_in  : in  DCache_in_type;
       DCache_out : out DCache_out_type);
end DCache;

architecture DCache_behave of DCache is
  signal Controller_in           : Controller_in_type;
  signal Controller_out          : Controller_out_type;
  signal LRU_Cal_in              : LRU_Cal_in_type;
  signal LRU_Cal_out             : LRU_Cal_out_type;
  signal LRU_Rep_in              : LRU_Rep_in_type;
  signal LRU_Rep_out             : LRU_Rep_out_type;
  signal PseudoRan_Rep_out       : PseudoRan_Rep_out_type;
  signal LRU_Table_in            : LRU_Table_in_type;
  signal LRU_Table_out           : LRU_Table_out_type;
  signal LRU_Table_zero          : std_logic_vector(DC_LRU_Table_Width-1 downto 0);
  type   DCache_Mem_in_array_type is array (DC_Num_Of_Ways-1 downto 0) of DCache_Mem_in_type;
  signal DCache_Mem_in_array     : DCache_Mem_in_array_type;
  type   DCache_Mem_out_array_type is array (DC_Num_Of_Ways-1 downto 0) of DCache_Mem_out_type;
  signal DCache_Mem_out_array    : DCache_Mem_out_array_type;
  signal One_Way_Data            : std_logic_vector (DC_Block_Size-1 downto 0);
  signal One_Way_Tag             : std_logic_vector (DC_Tag_Size-1 downto 0);
  signal Tag_plus_Vbit           : std_logic_vector(DC_Tag_Size downto 0);  --contains Tag and valid bit
  signal tag_data_last           : std_logic_vector(DC_Tag_Size downto 0);
  signal tag_data_zero           : std_logic_vector(DC_Tag_Size downto 0) := (others => '0');
  signal Block_offset_integer    : natural range 0 to Words_in_Block-1;
  signal tag_addr                : std_logic_vector (DC_Index_in_Bits-1 downto 0);
  signal tag_addr_last           : std_logic_vector (DC_Index_in_Bits-1 downto 0);
  signal en_Way_Wr               : std_logic_vector(DC_Num_Of_Ways-1 downto 0);
  signal LRU_Wr_addr             : std_logic_vector(DC_Index_in_Bits-1 downto 0);
  signal First_Addr_Block        : unsigned(Address_Space_DMem-1 downto 0);  --points to first address of a block in memory
  signal First_Addr_Block_WrBack : unsigned(Address_Space_DMem-1 downto 0);
  signal zero_offset             : unsigned(integer(log2(real(Num_DMem_Refer*4)))-1 downto 0);
  signal Tag_Plus_Index_WrBack   : std_logic_vector(DC_Tag_Size+DC_Index_in_Bits-1 downto 0);
  file F                         : text open write_mode is "output";
  file MLOG                      : text open write_mode is "memory_log";
  signal Re_Addr2DMem            : std_logic_vector(Address_Space_DMem-1 downto 0);
  signal Wr_Addr2DMem            : std_logic_vector(Address_Space_DMem-1 downto 0);
  signal ALU                     : std_logic_vector(Data_Bus_Width-1 downto 0);
  signal CPU_Wr                  : std_logic;
  signal CPU_Re                  : std_logic;
  
begin
  registers: process (clk, Reset)
  begin
    if rising_edge(clk) then
      if Reset = '0' then
        ALU <= (others => '0');
        CPU_Wr <= '0';
        CPU_Re <= '0';
      elsif DCache_in.DFetch_Stopped = '0' then
        ALU    <= DCache_in.ALU;
        CPU_Wr <= DCache_in.CPU_Wr; 
        CPU_Re <= DCache_in.CPU_Re;
      end if;
    end if;
  end process registers;

  
  DC_LRU_gen : if(DC_Rep_Policy = LRU and DC_Num_Of_Ways /= 1) generate
    LRU_Cal_in.LRU_Cur <= LRU_Table_out.DataO;

    MyDC_LRU_Cal : DC_LRU_Cal
      generic map(DC_Num_Of_Ways)
      port map(LRU_Cal_in  => LRU_Cal_in,
               LRU_Cal_out => LRU_Cal_out);
    LRU_Rep_in.LRU_Cur <= LRU_Table_out.DataO;

    MyDC_LRU_Rep : DC_LRU_Rep
      generic map(DC_Num_Of_Ways)
      port map(LRU_Rep_in  => LRU_Rep_in,
               LRU_Rep_out => LRU_Rep_out);

    LRU_Table_in.en_LRU    <= (Controller_out.en_flush or (Controller_out.en_cache_Tag and not(Controller_out.write_cache_Tag)));
    LRU_Table_in.Read_Addr <= tag_addr;
    LRU_Table_in.Wr_LRU    <= (Controller_out.en_flush or Controller_in.hit);

    MyDC_LRU_Table : DC_LRU_Table
      generic map(DC_Num_Of_Sets,
                  DC_LRU_Table_Width)
      port map(clk           => clk,
               LRU_Table_in  => LRU_Table_in,
               LRU_Table_out => LRU_Table_out);
    LRU_Table_zero <= (others => '0');

    process (clk)
    begin
      if(rising_edge(clk))then
        LRU_Wr_addr <= LRU_Table_in.Read_Addr;
      end if;
    end process;

    --MUX to select write address of DC_LRU Table
    LRU_Table_in.Wr_Addr <= LRU_Wr_addr        when (Controller_out.en_flush = '0') else std_logic_vector(to_unsigned(Controller_out.counter_flush, DC_Index_in_Bits));
    --MUX to select write data of DC_LRU Table
    LRU_Table_in.DataI   <= LRU_Cal_out.LRU_Ne when (Controller_out.en_flush = '0') else LRU_Table_zero;

    process(LRU_Rep_out.en_Way, Controller_out.en_flush)
    begin
      for i in 0 to DC_Num_Of_Ways-1 loop
        en_Way_Wr(i) <= (LRU_Rep_out.en_Way(i) or Controller_out.en_flush);
      end loop;
    end process;
  end generate;

  PseudoRan_gen : if (DC_Rep_Policy = PseudoRan and DC_Num_Of_Ways /= 1) generate
    MyDC_PseudoRan_Rep : DC_PseudoRan_Rep
      generic map(DC_Num_Of_Ways,
                  LFSR_Width)       
      port map(Clk               => clk,
               Reset             => Reset,
               PseudoRan_Rep_out => PseudoRan_Rep_out);

    process(clk)
    begin
      if(rising_edge(clk))then
        if(Controller_out.en_flush = '1' or (Controller_out.en_cache_Tag = '1' and Controller_out.write_cache_Tag = '0')) then
          for i in 0 to DC_Num_Of_Ways-1 loop
            en_Way_Wr(i) <= (PseudoRan_Rep_out.en_Way(i) or Controller_out.en_flush);
          end loop;
        end if;
      end if;
    end process;
  end generate;

  -- direct map
  Dir_Map_gen : if(DC_Num_Of_Ways = 1) generate
    en_Way_Wr(0) <= '1';
  end generate;

  -- Generate the ways of data cache
  DCache_Mem_Inst : for i in 0 to DC_Num_Of_Ways-1 generate
  begin
    Input_DCache_Mem_Init : for j in 0 to DC_Num_Data_Mem-1 generate
      DCache_Mem_in_array(i).DCache_DataI_Block(j) <= DCache_in.DMem_Data(((j mod DMem2DataWRatio)+1)*DC_Data_Mem_Width-1 downto (j mod DMem2DataWRatio)*DC_Data_Mem_Width)
                                                      when Controller_in.hit = '0' else
                                                      DCache_in.CPU_Data(((j mod DC_Bytes_in_Word)+1)*DC_Data_Mem_Width-1 downto (j mod DC_Bytes_in_Word)*DC_Data_Mem_Width);
      DCache_Mem_in_array(i).write_cache_Data(j)   <= (Controller_out.write_cache_Data(j) and en_Way_Wr(i))                                                             when Controller_in.hit = '0'
                                                      else (Controller_out.write_cache_Data(j) and DCache_Mem_out_array(i).hit);
    end generate;
    
    DCache_Mem_in_array(i).en_cache_Data   <= Controller_out.en_cache_Data;
    DCache_Mem_in_array(i).write_cache_Tag <= Controller_out.write_cache_Tag and en_Way_Wr(i);
    DCache_Mem_in_array(i).en_cache_Tag    <= Controller_out.en_cache_Tag;
    DCache_Mem_in_array(i).en_DirtBit      <= (Controller_out.en_DirtBit and en_Way_Wr(i))
                                              when Controller_in.hit = '0' else
                                              (Controller_out.en_DirtBit and DCache_Mem_out_array(i).hit);
    DCache_Mem_in_array(i).DirtBit_DataI <= Controller_out.DirtBit_DataI;
    DCache_Mem_in_array(i).Set_Addr      <= tag_addr_last;
    DCache_Mem_in_array(i).Tag_DataI     <= tag_data_last;

    MyDCache_Mem : DCache_Mem
      generic map(DC_Tag_Size,
                  DC_Index_in_Bits,
                  DC_Num_Data_Mem,
                  DC_Data_Mem_Width)         
      port map(clk            => clk,
               Reset          => Reset,                      -- active low
               DCache_Mem_in  => DCache_Mem_in_array(i),
               DCache_Mem_out => DCache_Mem_out_array(i));

    LRU_Cal_in.hit(i) <= DCache_Mem_out_array(i).hit;
  end generate;

  Controller_in.ready_DMem       <= DCache_in.ready_DMem;
  Controller_in.CPU_Re_comb      <= DCache_in.CPU_Re;
  Controller_in.CPU_Wr_comb      <= DCache_in.CPU_Wr;
  Controller_in.CPU_Re           <= CPU_Re;
  Controller_in.CPU_Wr           <= CPU_Wr;
  Controller_in.Mask             <= DCache_in.MaskI;
  Controller_in.Extr_Stall       <= DCache_in.Extr_Stall;
  Controller_in.Block_Offset_int <= Block_offset_integer;

  WrThrNoAlloc_Contro_gen : if (DC_Write_Policy = WrThrNoAlloc) generate
    MyDC_Controller_WrThrNoAlloc : DC_Controller_WrThrNoAlloc
      generic map(Num_DMem_Refer,
                  DMem2DataWRatio,
                  DC_Num_Of_Sets,
                  DC_Bytes_in_Word)
      port map(clk            => clk,
               Reset          => Reset,                      -- active low
               Controller_in  => Controller_in,
               Controller_out => Controller_out);  
  end generate;

  WrThrAlloc_Contro_gen : if (DC_Write_Policy = WrThrAlloc) generate
    MyDC_Controller_WrThrAlloc : DC_Controller_WrThrAlloc
      generic map(Num_DMem_Refer,
                  DMem2DataWRatio,
                  DC_Num_Of_Sets,
                  DC_Bytes_in_Word)
      port map(clk            => clk,
               Reset          => Reset,                      -- active low
               Controller_in  => Controller_in,
               Controller_out => Controller_out);  
  end generate;

  WrBackAlloc_Contro_gen : if (DC_Write_Policy = WrBackAlloc) generate
    MyDC_Controller_WrBackAlloc : DC_Controller_WrBackAlloc
      generic map(Num_DMem_Refer,
                  DMem2DataWRatio,
                  DC_Num_Of_Sets,
                  DC_Bytes_in_Word)
      port map(clk            => clk,
               Reset          => Reset,                      -- active low
               Controller_in  => Controller_in,
               Controller_out => Controller_out);  
  end generate;

  Tag_plus_Vbit        <= '1' & ALU(Data_Bus_Width-1 downto Data_Bus_Width-DC_Tag_Size);
  tag_data_zero        <= (others => '0');
  Block_offset_integer <= to_integer(unsigned(ALU(DC_Block_Offset_in_bits+DC_Byte_Offset_in_Bits-1
                                                            downto DC_Byte_Offset_in_Bits)));
  -- Mux to choose Cache address
  tag_addr             <= DCache_in.ALU(DC_Index_in_Bits+DC_Block_Offset_in_bits+DC_Byte_Offset_in_Bits-1
                                             downto DC_Block_Offset_in_bits+DC_Byte_Offset_in_Bits)
                          when (DCache_in.DFetch_Stopped = '0') else
                          ALU(DC_Index_in_Bits+DC_Block_Offset_in_bits+DC_Byte_Offset_in_Bits-1
                                        downto DC_Block_Offset_in_bits+DC_Byte_Offset_in_Bits);
  --MUX to select write address of Tag memory
  tag_addr_last        <= tag_addr when (Controller_out.en_flush = '0') else
                          std_logic_vector(to_unsigned(Controller_out.counter_flush, DC_Index_in_Bits));
  --MUX to select write data of Tag memory
  tag_data_last        <= Tag_plus_Vbit when (Controller_out.en_flush = '0') else
                          tag_data_zero;

  DataO_DCache_WrThr_gen : if(DC_Write_Policy = WrThrNoAlloc or DC_Write_Policy = WrThrAlloc) generate
    process(DCache_Mem_out_array)
    begin
      Controller_in.hit <= '0';
      One_Way_Data      <= (others => '0');
      for i in 0 to DC_Num_Of_Ways-1 loop
        if(DCache_Mem_out_array(i).hit = '1') then
          Controller_in.hit <= '1';
          One_Way_Data      <= DCache_Mem_out_array(i).DCache_DataO_Block;
          exit;
        end if;
      end loop;
    end process;
  end generate;

  DataO_DCache_WrBackAlloc_gen : if(DC_Write_Policy = WrBackAlloc) generate
    process(DCache_Mem_out_array, en_Way_Wr, Controller_in.hit)
    begin
      Controller_in.hit           <= '0';
      One_Way_Data                <= (others => '0');
      One_Way_Tag                 <= (others => '0');
      Controller_in.DirtBit_DataO <= '0';

      for i in 0 to DC_Num_Of_Ways-1 loop
        if(DCache_Mem_out_array(i).hit = '1') then
          Controller_in.hit <= '1';
          exit;
        end if;
      end loop;

      for i in 0 to DC_Num_Of_Ways-1 loop
          if(DCache_Mem_out_array(i).hit = '1') then
            One_Way_Data <= DCache_Mem_out_array(i).DCache_DataO_Block;
            exit;
          end if;
      end loop;

      for i in 0 to DC_Num_Of_Ways-1 loop
          if(en_Way_Wr(i) = '1' and Controller_in.hit = '0')then  -- Way that is candidate for replacement should be written back to data memory
            One_Way_Data                <= DCache_Mem_out_array(i).DCache_DataO_Block;
            One_Way_Tag                 <= DCache_Mem_out_array(i).Tag_DataO;
            Controller_in.DirtBit_DataO <= DCache_Mem_out_array(i).DirtBit_DataO;
            exit;
          end if;
      end loop;
    end process;
  end generate;

  DCache_out.DCache_DataO_Word <= One_Way_Data(Word_Size*(Block_Offset_integer+1)-1 downto Word_Size*Block_Offset_integer);
  DCache_out.Re_DMem           <= Controller_out.Re_DMem;
  DCache_out.Wr_DMem           <= Controller_out.Wr_DMem;
  DCache_out.ready_DCache      <= Controller_out.ready_DCache;
  DCache_out.Mem_Acc_Finished  <= Controller_out.Mem_Acc_Finished;

  Data2DMemWrThr_gen : if(DC_Write_Policy = WrThrNoAlloc or DC_Write_Policy = WrThrAlloc) generate
    DCache_out.Data2DMem <= DCache_in.CPU_Data;
  end generate;

  Data2DMemWrBackAlloc_gen : if(DC_Write_Policy = WrBackAlloc) generate
    DCache_out.Data2DMem <= One_Way_Data(DMemWidth*(Controller_out.counter_Mem_Wr_data+1)-1 downto DMemWidth*Controller_out.counter_Mem_Wr_data);
  end generate;

  Masko_WrThr_gen : if(DC_Write_Policy = WrThrNoAlloc or DC_Write_Policy = WrThrAlloc) generate
    DCache_out.MaskO <= DCache_in.MaskI;
  end generate;

  Masko_WrBackAlloc_gen : if(DC_Write_Policy = WrBackAlloc) generate
    DCache_out.MaskO <= (others => '1');
  end generate;

  zero_offset             <= (others => '0');
  First_Addr_Block        <= unsigned(ALU(Address_Space_DMem-1 downto DC_Block_Offset_in_bits+DC_Byte_Offset_in_Bits)) & zero_offset;
  Tag_Plus_Index_WrBack   <= One_Way_Tag & ALU(Data_Bus_Width-DC_Tag_Size-1 downto Data_Bus_Width-DC_Tag_Size-DC_Index_in_Bits);
  First_Addr_Block_WrBack <= unsigned(Tag_Plus_Index_WrBack(Address_Space_DMem-integer(log2(real(Num_DMem_Refer*4)))-1 downto 0)) & zero_offset;
  Re_Addr2DMem            <= std_logic_vector(First_Addr_Block+to_unsigned(Controller_out.counter_Mem_Re*4, Address_Space_DMem));
  Wr_Addr2DMem            <= std_logic_vector(First_Addr_Block_WrBack+to_unsigned(Controller_out.counter_Mem_wr*4, Address_Space_DMem));

  Addr2DMemWrThrNoAlloc_gen : if(DC_Write_Policy = WrThrNoAlloc) generate
    DCache_out.Address2DMem <= Re_Addr2DMem when(CPU_Wr = '0') else
                               ALU(Address_Space_DMem-1 downto 0);
  end generate;
  
  Addr2DMemWrThrAlloc_gen : if(DC_Write_Policy = WrThrAlloc) generate
    DCache_out.Address2DMem <= Re_Addr2DMem when (Controller_in.hit = '0') else
                               ALU(Address_Space_DMem-1 downto 0);
  end generate;

  Addr2DMemWrBackAlloc_gen : if(DC_Write_Policy = WrBackAlloc) generate
    DCache_out.Address2DMem <= Re_Addr2DMem when (Controller_out.WrBack = '0') else
                               Wr_Addr2DMem;
  end generate;

  -- This generate is only for testing DCache and is not part of DCache
--  output_file_WrBack_gen : if(DC_Write_Policy = WrBackAlloc) generate  
--    print : process(Clk)
--      variable L : line;
--    begin
--      if rising_edge(Clk) then
--        if (CPU_Wr = '1' and Controller_in.hit = '1' and TO_INTEGER(unsigned(tag_addr_last)) = 0) then
--          -- Write int
--          if (TO_INTEGER(unsigned(Controller_out.write_cache_Data(3 downto 0))) = 15 and TO_INTEGER(unsigned(ALU)) = 0) then
--            assert false report "Wrote integer" severity note;
--            write(L, TO_INTEGER(unsigned(DCache_in.CPU_Data)));
--            writeline(F, L);
--          end if;
--          --  Write char
--          if (Controller_out.write_cache_Data(7) = '1' and TO_INTEGER(unsigned(ALU)) = 4) then
--            assert false report "Wrote character" severity note;
--            write(L, TO_INTEGER(unsigned(DCache_in.CPU_Data(31 downto 24))));
--            write(L, string'(" char"));
--            writeline(F, L);
--          end if;
--        end if;
--      end if;
--    end process print;
--
--    lalal : process (Clk)
--      variable L : line;
--    begin
--      if rising_edge(Clk) then
--        if (Controller_out.DirtBit_DataI = '1' and Controller_out.en_DirtBit = '1') then
--          write(L, TO_INTEGER(unsigned(DCache_in.CPU_Data)));
--          writeline(MLOG, L);
--        end if;
--      end if;
--    end process lalal;
--  end generate;
end DCache_behave;






