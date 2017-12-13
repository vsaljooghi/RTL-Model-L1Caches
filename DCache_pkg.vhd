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

package dcache_pkg is

  type Controller_in_type is record
    ready_DMem       : std_logic;
    CPU_Wr           : std_logic;
    CPU_Re           : std_logic;
    CPU_Wr_comb      : std_logic;
    CPU_Re_comb      : std_logic;
    hit              : std_logic;
    Mask             : std_logic_vector (DC_Bytes_in_Word-1 downto 0);
    Block_Offset_int : natural;
    DirtBit_DataO    : std_logic;
    Extr_Stall       : std_logic;
  end record;

  type Controller_out_type is record
    counter_Mem_Re      : natural range 0 to Num_DMem_Refer;
    counter_Mem_Wr      : natural range 0 to Num_DMem_Refer-1;
    counter_Mem_Wr_data : natural range 0 to Num_DMem_Refer-1;
    counter_flush       : natural range 0 to DC_Num_of_Sets;
    en_flush            : std_logic;
    en_cache_Data       : std_logic_vector(DC_Num_Data_Mem-1 downto 0);
    write_cache_Data    : std_logic_vector(DC_Num_Data_Mem-1 downto 0);
    en_cache_Tag        : std_logic;
    write_cache_Tag     : std_logic;
    ready_DCache        : std_logic;
    Wr_DMem             : std_logic;
    Re_DMem             : std_logic;
    en_DirtBit          : std_logic;
    DirtBit_DataI       : std_logic;
    WrBack              : std_logic;
    Mem_Acc_Finished    : std_logic;
  end record;
  
  type DataMemByteVec_type is array (0 to DC_Block_Size_in_Byte-1) of std_logic_vector(7 downto 0);  -- array of bytes in Data memory of DCache

  type DCache_Mem_in_type is record
    write_cache_Data   : std_logic_vector(DC_Num_Data_Mem-1 downto 0);
    en_cache_Data      : std_logic_vector(DC_Num_Data_Mem-1 downto 0);
    write_cache_Tag    : std_logic;
    en_cache_Tag       : std_logic;
    DCache_DataI_Block : DataMemByteVec_type;
    Set_Addr           : std_logic_vector (DC_Index_in_Bits-1 downto 0);
    Tag_DataI          : std_logic_vector (DC_Tag_Size downto 0);
    en_DirtBit         : std_logic;
    DirtBit_DataI      : std_logic;
  end record;

  type DCache_Mem_out_type is record
    DCache_DataO_Block : std_logic_vector (DC_Block_Size-1 downto 0);
    Tag_DataO          : std_logic_vector (DC_Tag_Size-1 downto 0);
    hit                : std_logic;
    DirtBit_DataO      : std_logic;
  end record;

  type DCache_in_type is record
    DMem_Data     : std_logic_vector (DMemWidth-1 downto 0);
    CPU_Data      : std_logic_vector (Data_Bus_Width-1 downto 0);
    ALU           : std_logic_vector (Data_Bus_Width-1 downto 0);
    CPU_Wr        : std_logic;
    CPU_Re        : std_logic;
    DFetch_Stopped : std_logic;
    ready_DMem    : std_logic;
    MaskI         : std_logic_vector (DC_Bytes_in_Word-1 downto 0);
    Extr_Stall    : std_logic;
  end record;

  type DCache_out_type is record
    Address2DMem      : std_logic_vector (Address_Space_DMem-1 downto 0);
    Data2DMem         : std_logic_vector (DMemWidth-1 downto 0);
    DCache_DataO_Word : std_logic_vector (Word_Size-1 downto 0);
    Re_DMem           : std_logic;
    Wr_DMem           : std_logic;
    ready_DCache      : std_logic;
    MaskO             : std_logic_vector (DC_Bytes_in_Word-1 downto 0);
    Mem_Acc_Finished  : std_logic;
  end record;

  type Data_Mem_in_type is record
    Data_en    : std_logic;
    Address    : std_logic_vector (DC_Index_in_Bits-1 downto 0);
    Write_Data : std_logic;
    DataI      : std_logic_vector (DC_Data_Mem_Width-1 downto 0);
  end record;

  type Data_Mem_out_type is record
    DataO : std_logic_vector (DC_Data_Mem_Width-1 downto 0);
  end record;

  type LRU_Cal_in_type is record
    LRU_Cur : std_logic_vector (DC_LRU_Table_Width-1 downto 0);
    hit     : std_logic_vector (DC_Num_Of_Ways-1 downto 0);
  end record;

  type LRU_Cal_out_type is record
    LRU_Ne : std_logic_vector (DC_LRU_Table_Width-1 downto 0);
  end record;

  type LRU_Rep_in_type is record
    LRU_Cur : std_logic_vector (DC_LRU_Table_Width-1 downto 0);
  end record;

  type LRU_Rep_out_type is record
    en_Way : std_logic_vector (DC_Num_Of_Ways-1 downto 0);
  end record;

  type LRU_Table_in_type is record
    en_LRU    : std_logic;
    Read_Addr : std_logic_vector (DC_Index_in_Bits-1 downto 0);
    Wr_Addr   : std_logic_vector (DC_Index_in_Bits-1 downto 0);
    Wr_LRU    : std_logic;
    DataI     : std_logic_vector (DC_LRU_Table_Width-1 downto 0);
  end record;

  type LRU_Table_out_type is record
    DataO : std_logic_vector (DC_LRU_Table_Width-1 downto 0);
  end record;

  type PseudoRan_Rep_out_type is record
    en_Way : std_logic_vector (DC_Num_Of_Ways-1 downto 0);
  end record;

  type Tag_Mem_in_type is record
    Tag_en    : std_logic;
    Address   : std_logic_vector (DC_Index_in_Bits-1 downto 0);
    Write_tag : std_logic;
    DataI     : std_logic_vector (DC_Tag_Size downto 0);
  end record;

  type Tag_Mem_out_type is record
    DataO : std_logic_vector (DC_Tag_Size downto 0);
  end record;

  type DirtBit_Mem_in_type is record
    en      : std_logic;
    Address : std_logic_vector (DC_Index_in_Bits-1 downto 0);
    DataI   : std_logic;
  end record;

  type DirtBit_Mem_out_type is record
    DataO : std_logic;
  end record;

  component DirtBit_Mem is
    generic(DC_Index_in_Bits:natural:=DC_Index_in_Bits);           
    port ( clk                                            : in  std_logic;
           Reset                                          : in  std_logic;
           DirtBit_Mem_in                              : in  DirtBit_Mem_in_type;        
           DirtBit_Mem_out                             : out DirtBit_Mem_out_type);
  end component DirtBit_Mem;

  component DC_Tag_Mem is
    generic(DC_Index_in_Bits:natural:=DC_Index_in_Bits;
            DC_Tag_Size      : natural := DC_Tag_Size);

    port (clk          : in  std_logic;
           Tag_Mem_in  : in  Tag_Mem_in_type;
           Tag_Mem_out : out Tag_Mem_out_type);
  end component DC_Tag_Mem;

  component DC_LRU_Rep is
    generic(DC_Num_Of_Ways : natural := DC_Num_Of_Ways);
    port (LRU_Rep_in  : in  LRU_Rep_in_type;
          LRU_Rep_out : out LRU_Rep_out_type);
  end component DC_LRU_Rep;

  component DC_LRU_Cal is
    generic(DC_Num_Of_Ways : natural := DC_Num_Of_Ways);
    port (LRU_Cal_in  : in  LRU_Cal_in_type;
          LRU_Cal_out : out LRU_Cal_out_type);
  end component DC_LRU_Cal;

  component DC_Data_Mem is
    generic(DC_Index_in_Bits:natural:=DC_Index_in_Bits;
            DC_Data_Mem_Width : natural := DC_Data_Mem_Width);
    port (clk           : in  std_logic;
           Data_Mem_in  : in  Data_Mem_in_type;
           Data_Mem_out : out Data_Mem_out_type);
  end component DC_Data_Mem;
  
  component DCache is
    generic(
      DC_Block_Size           : natural;
      Address_Space_CPU       : natural;
      Address_Space_DMem      : natural;
      Word_Size               : natural;
      DMemWidth              : natural;
      DC_Block_Offset_in_bits : natural;
      DC_Tag_Size             : natural;
      DC_Index_in_Bits        : natural;
      DC_Num_Data_Mem         : natural;
      DC_Data_Mem_Width       : natural;
      DMem2DataWRatio         : natural;
      Num_DMem_Refer          : natural;
      DC_LRU_Table_Width      : natural;
      DC_Num_Of_Ways          : natural;
      DC_Num_Of_Sets          : natural;
      DC_Rep_Policy           : natural;
      LRU                     : natural);
    port(
      clk        : in  std_logic;
      Reset      : in  std_logic;
      DCache_in  : in  DCache_in_type;
      DCache_out : out DCache_out_type);
  end component;

  component DC_LRU_Table is
    generic(DC_Num_Of_Sets     : natural := DC_Num_Of_Sets;
            DC_LRU_Table_Width : natural := DC_LRU_Table_Width);          
    port (clk           : in  std_logic;
          LRU_Table_in  : in  LRU_Table_in_type;
          LRU_Table_out : out LRU_Table_out_type);
  end component DC_LRU_Table;

  component DC_PseudoRan_Rep is
    generic(DC_Num_Of_Ways : natural := DC_Num_Of_Ways;
            LFSR_Width     : natural := LFSR_Width);       
    port (Clk               : in  std_logic;
          Reset             : in  std_logic;
          PseudoRan_Rep_out : out PseudoRan_Rep_out_type);
  end component DC_PseudoRan_Rep;

  component DCache_Mem is
    generic(DC_Tag_Size       : natural := DC_Tag_Size;
            DC_Index_in_Bits  : natural := DC_Index_in_Bits;
            DC_Num_Data_Mem   : natural := DC_Num_Data_Mem;
            DC_Data_Mem_Width : natural := DC_Data_Mem_Width);         
    port (clk            : in  std_logic;
          Reset          : in  std_logic;  -- active low
          DCache_Mem_in  : in  DCache_Mem_in_type;
          DCache_Mem_out : out DCache_Mem_out_type);
  end component DCache_Mem;

  component DC_Controller_WrThrNoAlloc is
    generic(Num_DMem_Refer   : natural := Num_DMem_Refer;
            DMem2DataWRatio  : natural := DMem2DataWRatio;
            DC_Num_Of_Sets   : natural := DC_Num_Of_Sets;
            DC_Bytes_in_Word : natural := DC_Bytes_in_Word);
    port (clk            : in  std_logic;
          Reset          : in  std_logic;  -- active low
          Controller_in  : in  Controller_in_type;
          Controller_out : out Controller_out_type);
  end component DC_Controller_WrThrNoAlloc;

  component DC_Controller_WrThrAlloc is
    generic(Num_DMem_Refer   : natural := Num_DMem_Refer;
            DMem2DataWRatio  : natural := DMem2DataWRatio;
            DC_Num_Of_Sets   : natural := DC_Num_Of_Sets;
            DC_Bytes_in_Word : natural := DC_Bytes_in_Word);
    port (clk            : in  std_logic;
          Reset          : in  std_logic;  -- active low
          Controller_in  : in  Controller_in_type;
          Controller_out : out Controller_out_type);
  end component DC_Controller_WrThrAlloc;

  component DC_Controller_WrBackAlloc is
    generic(Num_DMem_Refer   : natural := Num_DMem_Refer;
            DMem2DataWRatio  : natural := DMem2DataWRatio;
            DC_Num_Of_Sets   : natural := DC_Num_Of_Sets;
            DC_Bytes_in_Word : natural := DC_Bytes_in_Word);
    port (clk            : in  std_logic;
          Reset          : in  std_logic;                    -- active low
          Controller_in  : in  Controller_in_type;
          Controller_out : out Controller_out_type);
  end component DC_Controller_WrBackAlloc;

end package;
