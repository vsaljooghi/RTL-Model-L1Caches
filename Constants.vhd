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
use ieee.math_real.all;
library work;
use work.common_pkg.all;

package constants is
-- Processor Constants
  constant Address_Space_CPU       : natural := 32;  -- CPU address length in bits 
  constant Data_Bus_Width          : natural := 32;  -- width of registers in register file , ALU output and ... 
  constant Ideal_Mem               : boolean := False;    -- If memory returns ready signal each clk cycle, it's an ideal one!
-- DMem   Constants:  
 -- constant DMem_Depth            : natural := 4294967296;
  constant DMemWidth              : natural := 32;
  constant Address_space_DMem      : natural := 32;  --natural(log2(real(DMem_Depth)));
-- IMem   Constants:  
--  constant IMem_Depth            : natural := 8388608;
  constant IMem_Width              : natural := 32;
  constant Address_space_IMem      : natural := 32; --natural(log2(real(IMem_Depth)));
-- common between DCache and ICache
  constant LRU                     : natural := 0;
  constant PseudoRan               : natural := 1;
  constant LFSR_Width              : natural := 6;
-- DCache Constants:   The prefix DC stands for Data cache
  constant WrThrNoAlloc            : natural := 0;
  constant WrThrAlloc              : natural := 1;
  constant WrBackAlloc             : natural := 2;
  constant DC_Write_Policy         : natural := WrThrNoAlloc;
  constant DC_Num_Of_Ways          : natural := 4;  -- Maximum 4
  constant DC_Rep_policy           : natural := LRU;  --LRU or pseudo random  LRU;
  constant DC_Num_Of_Sets          : natural := 32;  --Depth of DCache 
  constant Words_in_Block          : natural := 4;  -- number of words in each DCache line 
  constant Word_Size               : natural := 32;  --length of word in bits 
  constant DC_Data_Mem_Width       : natural := 8;  -- length of each chunk of Data part of DCache, Since we don't have buffer nor memory with mask, DMem_Width is always greater than or equal to Data_Mem_Width
  constant DC_LRU_Table_Width      : natural := natural(ceil(log2(real(factorial(DC_Num_Of_Ways)))));
  constant DC_Index_in_Bits        : natural := natural(log2(real(DC_Num_Of_Sets)));  --number of bits for Cache index 
  constant DC_Block_Offset_in_Bits : natural := natural(log2(real(Words_in_Block)));  -- number of bits for offset  
  constant DC_Block_Size           : natural := Words_in_Block*Word_Size;  -- block size in bits
  constant DC_Bytes_in_Word        : natural := Word_Size/8;
  constant DC_Byte_Offset_in_Bits  : natural := natural(log2(real(DC_Bytes_in_Word)));
  constant DC_Block_Size_in_Byte   : natural := DC_Block_Size/8;
  constant DC_Tag_Size             : natural := Data_Bus_Width-DC_Index_in_Bits-DC_Block_Offset_in_Bits-DC_Byte_Offset_in_Bits;
  constant DC_Num_Data_Mem         : natural := DC_Block_Size/DC_Data_Mem_Width;
  constant DMem2DataWRatio         : natural := DMemWidth/DC_Data_Mem_Width;
  constant Num_DMem_Refer          : natural := DC_Block_Size/DMemWidth;
--ICache Constants:  The prefix IC stands for Instruction Cache
  constant IC_Num_Of_Ways          : natural := 4;  -- Maximum 4
  constant IC_Rep_policy           : natural := LRU;  --LRU or pseudo random
  constant IC_Num_Of_Sets          : natural := 4;  --Depth of ICache
  constant Instr_in_Block          : natural := 4;  -- number of instructions in each ICache line
  constant Instr_Size              : natural := 32;  --length of instruction in bits 
  constant IC_Data_Mem_Width       : natural := 32;  --  length of each chunk of Data part of ICache,Since we don't have buffer nor memory with mask, IMem_Width is always greater than or equal to IC_Data_Mem_Width
  constant IC_LRU_Table_Width      : natural := natural(ceil(log2(real(factorial(IC_Num_Of_Ways)))));
  constant IC_Offset_in_bits       : natural := natural(log2(real(Instr_in_Block)));  -- number of bits for offset  
  constant IC_Block_Size           : natural := Instr_in_Block*Instr_Size;  -- block size in bits
  constant IC_Index_in_Bits        : natural := natural(log2(real(IC_Num_Of_Sets)));  --number of bits for ICache index    
  constant IC_Tag_Size             : natural := Address_Space_CPU-IC_Index_in_Bits-IC_Offset_in_bits;
  constant IC_Num_Data_Mem         : natural := IC_Block_Size/IC_Data_Mem_Width;
  constant IMem2DataWRatio         : natural := IMem_Width/IC_Data_Mem_Width;
  constant Num_IMem_Refer          : natural := IC_Block_Size/IMem_Width;
end package constants;
