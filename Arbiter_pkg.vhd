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
use work.cache_pkg.all;

package arbiter_pkg is
  -- Records for communicating between IC and Arbiter
  type arbiter_ic_in_record is record
    addr : std_logic_vector(Address_Space_IMem-1 downto 0);
    rd   : std_logic;
    acc  : std_logic;
  end record;

  type arbiter_ic_out_record is record
    data  : std_logic_vector(IMem_Width-1 downto 0);
    ready : std_logic;
  end record;

  -- Records for communicating between DC and Arbiter
  type arbiter_dc_in_record is record
    addr : std_logic_vector(Address_Space_DMem-1 downto 0);
    data : std_logic_vector(DMemWidth-1 downto 0);    
    mask : std_logic_vector(DMemWidth-1 downto 0);
    rd   : std_logic;
    wr   : std_logic;
    acc  : std_logic;
  end record;

  type arbiter_dc_out_record is record
    data  : std_logic_vector(DMemWidth-1 downto 0);    
    ready : std_logic;
  end record;

  component arbiter
    port (
      Clk             : in  std_logic;
      Reset           : in  std_logic;
      arbiter_ic_in   : in  arbiter_ic_in_record;
      arbiter_ic_out  : out arbiter_ic_out_record;
      arbiter_dc_in   : in  arbiter_dc_in_record;
      arbiter_dc_out  : out arbiter_dc_out_record;
      arbiter_mem_in  : in  mem_in_record;
      arbiter_mem_out : out mem_out_record);
  end component;

end arbiter_pkg;
