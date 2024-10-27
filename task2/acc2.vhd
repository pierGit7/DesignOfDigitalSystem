-- -----------------------------------------------------------------------------
--
--  Title      :  Edge-Detection design project - task 2.
--             :
--  Developers :  YOUR NAME HERE - s??????@student.dtu.dk
--             :  YOUR NAME HERE - s??????@student.dtu.dk
--             :
--  Purpose    :  This design contains an entity for the accelerator that must be build
--             :  in task two of the Edge Detection design project. It contains an
--             :  architecture skeleton for the entity as well.
--             :
--  Revision   :  1.0   ??-??-??     Final version
--             :
--
-- -----------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- The entity for task two. Notice the additional signals for the memory.
-- reset is active high.
--------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.types.all;

entity acc is
    port(
        clk    : in  bit_t;             -- The clock.
        reset  : in  bit_t;             -- The reset signal. Active high.
        addr   : out halfword_t;        -- Address bus for data.
        dataR  : in  word_t;            -- The data bus.
        dataW  : out word_t;            -- The data bus.
        en     : out bit_t;             -- Request signal for data.
        we     : out bit_t;             -- Read/Write signal for data.
        start  : in  bit_t;
        finish : out bit_t
    );
end acc;

--------------------------------------------------------------------------------
-- The desription of the accelerator.
--------------------------------------------------------------------------------

architecture rtl of acc is

-- All internal signals are defined here

type state_type is ( idle, invert, write, check_end);

signal state, next_state : state_type := idle;
signal reg, next_reg : halfword_t := halfword_zero;

begin
    cl : process(start,state)
    begin
        next_state <= state;
        next_reg <= reg;
        finish <= '0';
        addr <= reg;
        
        --addr <= halfword_zero;
    
        case (state) is
            when idle =>
                if start = '1' then
                    en <= '1';
                    next_state <= invert;
                    --addr <= reg;
                end if;
            when invert =>
                dataW <= not(dataR);
                next_reg <= std_logic_vector(unsigned(reg) + 25344);
                next_state <= write;
            when write =>
                we <= '1';
                next_state <= check_end;
            when check_end =>
            -- 25343 means this was the last word to read
                if (to_integer(unsigned(reg)) - 25344) /= 25343 then
                    we <= '0';  
                    next_reg <= std_logic_vector(unsigned(reg) - 25343);
                else
                    we <= '0';
                    en <= '0';
                    finish <= '1';
                end if;
                next_state<= idle;
        end case;    
        
        
    end process cl;

-- Template for a process
    register_process : process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                addr <= (others => '0');
                state <= idle;
                reg <= (others => '0');
                --en <= '0';
                --we <= '0';
            else
                -- Registers update
                state <= next_state;
                reg <= next_reg;
                addr <= next_reg;
            end if;
        end if;
    end process register_process;

end rtl;
