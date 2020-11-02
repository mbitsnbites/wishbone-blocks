----------------------------------------------------------------------------------------------------
-- Copyright (c) 2020 Marcus Geelnard
--
-- This software is provided 'as-is', without any express or implied warranty. In no event will the
-- authors be held liable for any damages arising from the use of this software.
--
-- Permission is granted to anyone to use this software for any purpose, including commercial
-- applications, and to alter it and redistribute it freely, subject to the following restrictions:
--
--  1. The origin of this software must not be misrepresented; you must not claim that you wrote
--     the original software. If you use this software in a product, an acknowledgment in the
--     product documentation would be appreciated but is not required.
--
--  2. Altered source versions must be plainly marked as such, and must not be misrepresented as
--     being the original software.
--
--  3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use work.test.all;
library vunit_lib;
context vunit_lib.vunit_context;

entity wb_ram_tb is
  generic (runner_cfg : string);
end wb_ram_tb;

architecture tb of wb_ram_tb is
  constant CLK_PERIOD : time := 1.0 ns;
  constant CLK_HALF_PERIOD : time := CLK_PERIOD / 2.0;

  signal s_rst : std_logic;
  signal s_clk : std_logic;
  signal s_adr : std_logic_vector(7 downto 0);
  signal s_dat_w : std_logic_vector(15 downto 0);
  signal s_we : std_logic;
  signal s_sel : std_logic_vector(1 downto 0);
  signal s_cyc : std_logic;
  signal s_stb : std_logic;
  signal s_dat : std_logic_vector(15 downto 0);
  signal s_ack : std_logic;
  signal s_stall : std_logic;
  signal s_rty : std_logic;
  signal s_err : std_logic;
begin
  -- DUT.
  ram : entity work.wb_ram
    generic map (
      ADR_WIDTH => s_adr'length,
      DAT_WIDTH => s_dat'length,
      GRANULARITY => s_dat'length / s_sel'length
    )
    port map (
      i_rst => s_rst,
      i_clk => s_clk,
      i_adr => s_adr,
      i_dat => s_dat_w,
      i_we => s_we,
      i_sel => s_sel,
      i_cyc => s_cyc,
      i_stb => s_stb,
      o_dat => s_dat,
      o_ack => s_ack,
      o_stall => s_stall,
      o_rty => s_rty,
      o_err => s_err
    );

  -- Test process.
  main : process
    type T_INPUTS is record
      adr : integer;
      dat : std_logic_vector(s_dat_w'range);
      we : std_logic;
      sel : std_logic_vector(s_sel'range);
      cyc : std_logic;
      stb : std_logic;
    end record;

    type T_OUTPUTS is record
      dat : std_logic_vector(s_dat'range);
      ack : std_logic;
      stall : std_logic;
      rty : std_logic;
      err : std_logic;
    end record;

    type T_SIGNALS is record
      i : T_INPUTS;
      o : T_OUTPUTS;
    end record;

    type T_SIGNALS_ARRAY is array (natural range <>) of T_SIGNALS;
    constant C_SIGNALS_ARRAY : T_SIGNALS_ARRAY := (
      -- Write to the memory.
      ((0, x"0005", '1', "11", '1', '1'), (x"----", '0', '0', '0', '0')),
      ((1, x"3b13", '1', "11", '1', '1'), (x"----", '1', '0', '0', '0')),
      ((2, x"0000", '1', "11", '1', '1'), (x"----", '1', '0', '0', '0')),
      ((2, x"3b13", '1', "10", '1', '1'), (x"----", '1', '0', '0', '0')),
      ((3, x"0000", '1', "11", '1', '1'), (x"----", '1', '0', '0', '0')),
      ((3, x"3b13", '1', "01", '1', '1'), (x"----", '1', '0', '0', '0')),
      ((4, x"0099", '1', "11", '1', '1'), (x"----", '1', '0', '0', '0')),
      ((5, x"ff00", '1', "11", '1', '1'), (x"----", '1', '0', '0', '0')),
      ((0, x"0000", '0', "11", '1', '0'), (x"----", '1', '0', '0', '0')),
      ((0, x"0000", '0', "11", '0', '0'), (x"----", '0', '0', '0', '0')),

      -- Read from the memory.
      ((0, x"0000", '0', "11", '1', '1'), (x"----", '0', '0', '0', '0')),
      ((1, x"0000", '0', "11", '1', '1'), (x"0005", '1', '0', '0', '0')),
      ((2, x"0000", '0', "10", '1', '1'), (x"3b13", '1', '0', '0', '0')),
      ((3, x"0000", '0', "01", '1', '1'), (x"3b00", '1', '0', '0', '0')),
      ((4, x"0000", '0', "11", '1', '1'), (x"0013", '1', '0', '0', '0')),
      ((5, x"0000", '0', "11", '1', '1'), (x"0099", '1', '0', '0', '0')),
      ((0, x"0000", '0', "11", '1', '0'), (x"ff00", '1', '0', '0', '0')),
      ((0, x"0000", '0', "11", '0', '0'), (x"----", '0', '0', '0', '0'))
    );

    variable v_in : T_INPUTS;
    variable v_out : T_OUTPUTS;
  begin
    test_runner_setup(runner, runner_cfg);
  
    -- Clear & reset.
    s_clk <= '0';
    s_rst <= '1';
    s_adr <= (others => '0');
    s_dat_w <= (others => '0');
    s_we <= '0';
    s_sel <= (others => '0');
    s_cyc <= '0';
    s_stb <= '0';
    wait for CLK_HALF_PERIOD;
    s_clk <= '1';
    wait for CLK_HALF_PERIOD;
    s_clk <= '0';
    s_rst <= '0';
    wait for CLK_HALF_PERIOD;
    s_clk <= '1';
    wait for CLK_HALF_PERIOD;
    s_clk <= '0';
    wait for CLK_HALF_PERIOD;
    s_clk <= '1';

    for k in C_SIGNALS_ARRAY'range loop
      wait until s_clk = '1';

      -- Set inputs.
      v_in := C_SIGNALS_ARRAY(k).i;
      s_adr <= to_vector(v_in.adr, s_adr'length);
      s_dat_w <= v_in.dat;
      s_we <= v_in.we;
      s_sel <= v_in.sel;
      s_cyc <= v_in.cyc;
      s_stb <= v_in.stb;

      wait for CLK_HALF_PERIOD;

      -- Check outputs.
      v_out := C_SIGNALS_ARRAY(k).o;
      check_match(s_dat, v_out.dat, result("for o_dat @ " & to_string(k)));
      check_equal(s_ack, v_out.ack, result("for o_ack @ " & to_string(k)));
      check_equal(s_stall, v_out.stall, result("for o_stall @ " & to_string(k)));
      check_equal(s_rty, v_out.rty, result("for o_rty @ " & to_string(k)));
      check_equal(s_err, v_out.err, result("for o_err @ " & to_string(k)));

      s_clk <= '0';
      wait for CLK_HALF_PERIOD;
      s_clk <= '1';
    end loop;

    -- End of simulation.
    test_runner_cleanup(runner);
  end process;
end architecture;
