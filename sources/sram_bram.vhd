----------------------------------------------------------------------------------
-- sram_bram.vhd
--
-- Copyright (C) 2007 Jonas Diemer
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin St, Fifth Floor, Boston, MA 02110, USA
--
----------------------------------------------------------------------------------
--
-- Details: http://www.sump.org/projects/analyzer/
--
-- Simple BlockRAM interface.
--
-- This module should be used instead of sram.vhd if no external SRAM is present.
-- Instead, it will use internal BlockRAM (16 Blocks).
--
-- Modified to use BRAM4k32bit to fit on Butterfly Platform
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity sram_bram is
	port(
		clock : in std_logic;
		output : out std_logic_vector(35 downto 0);
		input : in std_logic_vector(35 downto 0);
		read : in std_logic;
		write : in std_logic
	);
end sram_bram;

architecture behavioral of sram_bram is

	component BRAM6k36bit -- SampleRAM
		port (
			clka: IN std_logic;
			wea: IN std_logic_VECTOR(0 downto 0);
			addra: IN std_logic_VECTOR(12 downto 0);
			dina: IN std_logic_VECTOR(35 downto 0);
			douta: OUT std_logic_VECTOR(35 downto 0)
		);
	end component;

	attribute box_type : boolean;
	attribute box_type of BRAM6k36bit: component is true;
	signal addra : std_logic_vector (12 downto 0) := (others => '0');
	signal writeSignal : std_logic_vector(0 downto 0);
	signal bramIn, bramOut : std_logic_vector (35 downto 0);

begin

	-- assign signals
	output <= bramOut;

	-- memory io interface state controller
	bramIn <= input;

	writeSignal(0) <= write;

	-- memory address controller
	process(clock)
	begin
		if rising_edge(clock) then
			if write = '1' then
				if addra >= (6*1024) - 1 then
					addra <= (others => '0');
				else
					addra <= addra + 1;
				end if;
			elsif read = '1' then
				if addra = "0" then
					addra <= std_logic_vector(to_unsigned((6*1024) - 1, addra'length));
				else
					addra <= addra - 1;
				end if;
			end if;
		end if;
	end process;

	 Inst_SampleRAM : BRAM6k36bit
	 port map (
		 clka => clock,
		 wea => writeSignal,
		 addra => addra,
		 dina => bramIn,
		 douta => bramOut
	 );

end behavioral;

