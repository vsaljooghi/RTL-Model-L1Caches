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
use work.Constants.all;

package icache_pkg is
  type ICache_Mem_in_type is record
    write_cache_Data : std_logic_vector(IC_Num_Data_Mem-1 downto 0);
    en_cache_Data    : std_logic_vector(IC_Num_Data_Mem-1 downto 0);
    write_cache_Tag  : std_logic;
    en_cache_Tag     : std_logic;
    IMem_Data        : std_logic_vector (IMem_Width-1 downto 0);
    Set_Addr         : std_logic_vector (IC_Index_in_Bits-1 downto 0);
    Tag_DataI        : std_logic_vector (IC_Tag_Size downto 0);
  end record;

  type ICache_Mem_out_type is record
    ICache_DataO_Block : std_logic_vector (IC_Block_Size-1 downto 0);
    hit                : std_logic;
  end record;

  type Tag_Mem_in_type is record
    Tag_en    : std_logic;
    Address   : std_logic_vector (IC_Index_in_Bits-1 downto 0);
    Write_tag : std_logic;
    DataI     : std_logic_vector (IC_Tag_Size downto 0);
  end record;

  type Tag_Mem_out_type is record
    DataO : std_logic_vector (IC_Tag_Size downto 0);
  end record;

  type LRU_Table_in_type is record
    en_LRU    : std_logic;
    Read_Addr : std_logic_vector (IC_Index_in_Bits-1 downto 0);
    Wr_Addr   : std_logic_vector (IC_Index_in_Bits-1 downto 0);
    Wr_LRU    : std_logic;
    DataI     : std_logic_vector (IC_LRU_Table_Width-1 downto 0);
  end record;

  type LRU_Table_out_type is record
    DataO : std_logic_vector (IC_LRU_Table_Width-1 downto 0);
  end record;

  type LRU_Rep_in_type is record
    LRU_Cur : std_logic_vector (IC_LRU_Table_Width-1 downto 0);
  end record;

  type LRU_Rep_out_type is record
    en_Way : std_logic_vector (IC_Num_Of_Ways-1 downto 0);
  end record;

  type LRU_Cal_in_type is record
    LRU_Cur : std_logic_vector (IC_LRU_Table_Width-1 downto 0);
    hit     : std_logic_vector (IC_Num_Of_Ways-1 downto 0);
  end record;

  type LRU_Cal_out_type is record
    LRU_Ne : std_logic_vector (IC_LRU_Table_Width-1 downto 0);
  end record;

  type Data_Mem_in_type is record
    Data_en    : std_logic;
    Address    : std_logic_vector (IC_Index_in_Bits-1 downto 0);
    Write_Data : std_logic;
    DataI      : std_logic_vector (IC_Data_Mem_Width-1 downto 0);
  end record;

  type Data_Mem_out_type is record
    DataO : std_logic_vector (IC_Data_Mem_Width-1 downto 0);
  end record;

  type Controller_in_type is record
    Extr_Stall : std_logic;
    ready_IMem : std_logic;
    hit        : std_logic;
  end record;

  type Controller_out_type is record
    counter_Mem      : natural range 0 to Num_IMem_Refer-1;
    counter_flush    : natural range 0 to IC_Num_of_Sets;
    Re_IMem          : std_logic;
    en_flush         : std_logic;
    en_cache_Data    : std_logic_vector(IC_Num_Data_Mem-1 downto 0);
    write_cache_Data : std_logic_vector(IC_Num_Data_Mem-1 downto 0);
    en_cache_Tag     : std_logic;
    write_cache_Tag  : std_logic;
    Mem_Acc_Finished : std_logic;
  end record;

  type ICache_in_type is record
    Extr_Stall    : std_logic;
    IMem_Data     : std_logic_vector (IMem_Width-1 downto 0);
    PC            : std_logic_vector (Address_Space_CPU-1 downto 0);
    IFetch_Stopped : std_logic;
    ready_IMem    : std_logic;
  end record;

  type ICache_out_type is record
    Address2IMem       : std_logic_vector (Address_Space_IMem-1 downto 0);
    ICache_DataO_Instr : std_logic_vector (Instr_Size-1 downto 0);
    Re_IMem            : std_logic;
    hit                : std_logic;
    Mem_Acc_Finished   : std_logic;
  end record;

  component ICache is
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
  end component;

  component IC_Controller is
    generic(Num_IMem_Refer  : natural := Num_IMem_Refer;
            IMem2DataWRatio : natural := IMem2DataWRatio);
    port (clk            : in  std_logic;
          Reset          : in  std_logic;  -- active low
          Controller_in  : in  Controller_in_type;
          Controller_out : out Controller_out_type);
  end component IC_Controller;

  component IC_Data_Mem is
    generic(IC_Index_in_Bits  : natural := IC_Index_in_Bits;
            IC_Data_Mem_Width : natural := IC_Data_Mem_Width);

    port (clk           : in  std_logic;
           Data_Mem_in  : in  Data_Mem_in_type;
           Data_Mem_out : out Data_Mem_out_type);
  end component IC_Data_Mem;

  component IC_LRU_Cal is
    generic(IC_Num_Of_Ways : natural := IC_Num_Of_Ways);
    port (LRU_Cal_in  : in  LRU_Cal_in_type;
          LRU_Cal_out : out LRU_Cal_out_type);
  end component IC_LRU_Cal;

  component IC_LRU_Rep is
    generic(IC_Num_Of_Ways : natural := IC_Num_Of_Ways);
    port (LRU_Rep_in  : in  LRU_Rep_in_type;
          LRU_Rep_out : out LRU_Rep_out_type);
  end component IC_LRU_Rep;

  component IC_LRU_Table is
    generic(IC_Num_Of_Sets     : natural := IC_Num_Of_Sets;
            IC_LRU_Table_Width : natural := IC_LRU_Table_Width);
    port (clk           : in  std_logic;
          LRU_Table_in  : in  LRU_Table_in_type;
          LRU_Table_out : out LRU_Table_out_type);
  end component IC_LRU_Table;

  component IC_Tag_Mem is
    generic(IC_Index_in_Bits : natural := IC_Index_in_Bits;
            IC_Tag_Size      : natural := IC_Tag_Size);

    port (clk          : in  std_logic;
           Tag_Mem_in  : in  Tag_Mem_in_type;
           Tag_Mem_out : out Tag_Mem_out_type);
  end component IC_Tag_Mem;

  type PseudoRan_Rep_out_type is record
    en_Way : std_logic_vector (IC_Num_Of_Ways-1 downto 0);
  end record;

  component IC_PseudoRan_Rep is
    generic(IC_Num_Of_Ways : natural := IC_Num_Of_Ways;
            LFSR_Width     : natural := LFSR_Width);       
    port (Clk                : in  std_logic;
           Reset             : in  std_logic;
           PseudoRan_Rep_out : out PseudoRan_Rep_out_type);
  end component IC_PseudoRan_Rep;

  component ICache_Mem is
    generic(IC_Tag_Size       : natural := IC_Tag_Size;
            IC_Index_in_Bits  : natural := IC_Index_in_Bits;
            IC_Num_Data_Mem   : natural := IC_Num_Data_Mem;
            IC_Data_Mem_Width : natural := IC_Data_Mem_Width;
            IMem2DataWRatio   : natural := IMem2DataWRatio);         
    port (clk            : in  std_logic;
          Reset          : in  std_logic;  -- active low
          ICache_Mem_in  : in  ICache_Mem_in_type;
          ICache_Mem_out : out ICache_Mem_out_type);
  end component ICache_Mem;

end package;
