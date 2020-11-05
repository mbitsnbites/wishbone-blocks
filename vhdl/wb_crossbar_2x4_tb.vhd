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
use ieee.numeric_std.all;
use work.test.all;
library vunit_lib;
context vunit_lib.vunit_context;

entity wb_crossbar_2x4_tb is
  generic (runner_cfg : string);
end wb_crossbar_2x4_tb;

architecture tb of wb_crossbar_2x4_tb is
  constant CLK_PERIOD : time := 1.0 ns;
  constant CLK_HALF_PERIOD : time := CLK_PERIOD / 2.0;

  constant ADR_WIDTH : integer := 8;
  constant DAT_WIDTH : integer := 16;

  type T_WB is record
    adr : std_logic_vector(ADR_WIDTH-1 downto 0);
    dat_w : std_logic_vector(DAT_WIDTH-1 downto 0);
    we : std_logic;
    sel : std_logic_vector(DAT_WIDTH/8-1 downto 0);
    cyc : std_logic;
    stb : std_logic;
    dat : std_logic_vector(DAT_WIDTH-1 downto 0);
    ack : std_logic;
    stall : std_logic;
    rty : std_logic;
    err : std_logic;
  end record;

  signal s_master_a : T_WB;
  signal s_master_b : T_WB;

  signal s_slave_0 : T_WB;
  signal s_slave_1 : T_WB;
  signal s_slave_2 : T_WB;
  signal s_slave_3 : T_WB;

  signal s_rst : std_logic;
  signal s_clk : std_logic;

  signal s_clk_count : unsigned(DAT_WIDTH-1 downto 0);
  signal s_dly_1 : std_logic_vector(1 downto 0);
  signal s_dly_2 : std_logic_vector(2 downto 0);

  signal s_prn : std_logic_vector(31 downto 0);

  function rnd(x : integer; prn : std_logic_vector) return std_logic is
    variable v_masked : std_logic_vector(31 downto 0);
    variable v_result : std_logic;
  begin
    v_masked := std_logic_vector(to_unsigned(x, 32)) and prn;
    v_result := '1';
    for k in 0 to 31 loop
      v_result := v_result xor v_masked(k);
    end loop;
    return v_result;
  end function;

begin
  -- DUT.
  dut : entity work.wb_crossbar_2x4
    generic map (
      ADR_WIDTH => ADR_WIDTH,
      DAT_WIDTH => DAT_WIDTH,
      GRANULARITY => 8,
      LOG2_MAX_PENDING_REQS => 2
    )
    port map (
      i_rst => s_rst,
      i_clk => s_clk,

      i_adr_a => s_master_a.adr,
      i_dat_a => s_master_a.dat_w,
      i_we_a => s_master_a.we,
      i_sel_a => s_master_a.sel,
      i_cyc_a => s_master_a.cyc,
      i_stb_a => s_master_a.stb,
      o_dat_a => s_master_a.dat,
      o_ack_a => s_master_a.ack,
      o_stall_a => s_master_a.stall,
      o_rty_a => s_master_a.rty,
      o_err_a => s_master_a.err,

      i_adr_b => s_master_b.adr,
      i_dat_b => s_master_b.dat_w,
      i_we_b => s_master_b.we,
      i_sel_b => s_master_b.sel,
      i_cyc_b => s_master_b.cyc,
      i_stb_b => s_master_b.stb,
      o_dat_b => s_master_b.dat,
      o_ack_b => s_master_b.ack,
      o_stall_b => s_master_b.stall,
      o_rty_b => s_master_b.rty,
      o_err_b => s_master_b.err,

      o_adr_0 => s_slave_0.adr,
      o_dat_0 => s_slave_0.dat_w,
      o_we_0 => s_slave_0.we,
      o_sel_0 => s_slave_0.sel,
      o_cyc_0 => s_slave_0.cyc,
      o_stb_0 => s_slave_0.stb,
      i_dat_0 => s_slave_0.dat,
      i_ack_0 => s_slave_0.ack,
      i_stall_0 => s_slave_0.stall,
      i_rty_0 => s_slave_0.rty,
      i_err_0 => s_slave_0.err,

      o_adr_1 => s_slave_1.adr,
      o_dat_1 => s_slave_1.dat_w,
      o_we_1 => s_slave_1.we,
      o_sel_1 => s_slave_1.sel,
      o_cyc_1 => s_slave_1.cyc,
      o_stb_1 => s_slave_1.stb,
      i_dat_1 => s_slave_1.dat,
      i_ack_1 => s_slave_1.ack,
      i_stall_1 => s_slave_1.stall,
      i_rty_1 => s_slave_1.rty,
      i_err_1 => s_slave_1.err,

      o_adr_2 => s_slave_2.adr,
      o_dat_2 => s_slave_2.dat_w,
      o_we_2 => s_slave_2.we,
      o_sel_2 => s_slave_2.sel,
      o_cyc_2 => s_slave_2.cyc,
      o_stb_2 => s_slave_2.stb,
      i_dat_2 => s_slave_2.dat,
      i_ack_2 => s_slave_2.ack,
      i_stall_2 => s_slave_2.stall,
      i_rty_2 => s_slave_2.rty,
      i_err_2 => s_slave_2.err,

      o_adr_3 => s_slave_3.adr,
      o_dat_3 => s_slave_3.dat_w,
      o_we_3 => s_slave_3.we,
      o_sel_3 => s_slave_3.sel,
      o_cyc_3 => s_slave_3.cyc,
      o_stb_3 => s_slave_3.stb,
      i_dat_3 => s_slave_3.dat,
      i_ack_3 => s_slave_3.ack,
      i_stall_3 => s_slave_3.stall,
      i_rty_3 => s_slave_3.rty,
      i_err_3 => s_slave_3.err
    );

  -- PRNG.
  process(s_rst, s_clk)
    variable v_feedback : std_logic;
  begin
    if s_rst = '1' then
      s_prn <= x"8654af40";
    elsif rising_edge(s_clk) then
      -- 32-bit LFSR taps: 31, 21, 1, 0
      v_feedback := not (s_prn(31) xor s_prn(21) xor s_prn(1) xor s_prn(0));
      s_prn <= s_prn(30 downto 0) & v_feedback;
    end if;
  end process;

  -- Update master & slave states.
  process(s_rst, s_clk)
    variable v_msb : std_logic_vector(1 downto 0);
  begin
    if s_rst = '1' then
      s_master_a.adr <= (others => '0');
      s_master_a.dat_w <= (others => '0');
      s_master_a.we <= '0';
      s_master_a.sel <= (others => '0');
      s_master_a.cyc <= '0';
      s_master_a.stb <= '0';

      s_master_b.adr <= (others => '0');
      s_master_b.dat_w <= (others => '0');
      s_master_b.we <= '0';
      s_master_b.sel <= (others => '0');
      s_master_b.cyc <= '0';
      s_master_b.stb <= '0';

      s_slave_0.dat <= (others => '0');
      s_slave_0.ack <= '0';
      s_slave_0.stall <= '0';
      s_slave_0.rty <= '0';
      s_slave_0.err <= '0';

      s_slave_1.dat <= (others => '0');
      s_slave_1.ack <= '0';
      -- NOTE: s_slave_1.stall is set asynchronously, so we can't clear it here.
      s_slave_1.rty <= '0';
      s_slave_1.err <= '0';

      s_slave_2.dat <= (others => '0');
      s_slave_2.ack <= '0';
      s_slave_2.stall <= '0';
      s_slave_2.rty <= '0';
      s_slave_2.err <= '0';

      s_slave_3.dat <= (others => '0');
      s_slave_3.ack <= '0';
      s_slave_3.stall <= '0';
      s_slave_3.rty <= '0';
      s_slave_3.err <= '0';

      s_clk_count <= (others => '0');
      s_dly_1 <= (others => '0');
      s_dly_2 <= (others => '0');
    elsif rising_edge(s_clk) then
      -- Master A activity.
      if (s_master_a.stb and s_master_a.stall) = '1' then
        -- We just re-send the last request (nothing to do here).
      else
        -- Send a new request.
        v_msb := rnd(12, s_prn) & rnd(7, s_prn);
        s_master_a.adr <= v_msb & std_logic_vector(resize(s_clk_count, ADR_WIDTH-2));
        s_master_a.dat_w <= std_logic_vector(to_unsigned(0, DAT_WIDTH) - s_clk_count);
        s_master_a.we <= rnd(23, s_prn);
        s_master_a.sel <= (others => '1');
        s_master_a.cyc <= '1';
        s_master_a.stb <= rnd(5, s_prn);
      end if;

      -- Master B activity.
      if (s_master_b.stb and s_master_b.stall) = '1' then
        -- We just re-send the last request (nothing to do here).
      else
        -- Send a new request.
        v_msb := rnd(14, s_prn) & rnd(6, s_prn);
        s_master_b.adr <= v_msb & std_logic_vector(resize(s_clk_count, ADR_WIDTH-2));
        s_master_b.dat_w <= std_logic_vector(to_unsigned(7, DAT_WIDTH) - s_clk_count);
        s_master_b.we <= rnd(30, s_prn);
        s_master_b.sel <= (others => '1');
        s_master_b.cyc <= '1';
        s_master_b.stb <= rnd(9, s_prn);
      end if;

      -- Each slave just sends some data back.
      s_slave_0.dat <= std_logic_vector(to_unsigned(0, DAT_WIDTH) + s_clk_count);
      s_slave_1.dat <= std_logic_vector(to_unsigned(1*256, DAT_WIDTH) + s_clk_count);
      s_slave_2.dat <= std_logic_vector(to_unsigned(2*256, DAT_WIDTH) + s_clk_count);
      s_slave_3.dat <= std_logic_vector(to_unsigned(3*256, DAT_WIDTH) + s_clk_count);

      -- Slave 0 responds directly.
      s_slave_0.ack <= s_slave_0.cyc and s_slave_0.stb;

      -- Slave 1 occasionally stalls.
      -- NOTE: The stall signal is set asynchronously.
      s_slave_1.ack <= s_slave_1.cyc and s_slave_1.stb and rnd(55, s_prn);

      -- Slave 2 has a 2-cycle delay.
      s_slave_2.ack <= s_dly_1(1);

      -- Slave 3 has a 3-cycle delay.
      s_slave_3.ack <= s_dly_2(2);

      -- Delay pipes for the ack signal of slaves 2 & 3.
      s_dly_1 <= s_dly_1(0) & (s_slave_2.cyc and s_slave_2.stb);
      s_dly_2 <= s_dly_2(1 downto 0) & (s_slave_3.cyc and s_slave_3.stb);

      s_clk_count <= s_clk_count + 1;
    end if;
  end process;

  -- Slave 1 occasionally stalls.
  -- NOTE: The stall signal needs to be set asynchronously.
  s_slave_1.stall <= s_slave_1.cyc and s_slave_1.stb and not rnd(55, s_prn);

  -- Test process.
  main : process
  begin
    test_runner_setup(runner, runner_cfg);
  
    -- Clear & reset.
    s_clk <= '0';
    s_rst <= '1';
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

    for k in 0 to 100 loop
      wait until s_clk = '1';
      wait for CLK_HALF_PERIOD;
      s_clk <= '0';
      wait for CLK_HALF_PERIOD;
      s_clk <= '1';
    end loop;

    -- End of simulation.
    test_runner_cleanup(runner);
  end process;
end architecture;
