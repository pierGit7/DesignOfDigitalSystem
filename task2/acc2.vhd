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
type state_type_t2 is (idle, read, computation);
--type buffer_array is array(0 to 2, 0 to 2) of byte_t;

-- buffer of two rows
type row_buffer_array is array(0 to 703) of byte_t;

-- signal state, next_state : state_type := idle;
-- signal computation_buffer : buffer_array := (others => (others => '0'));

signal rows_buffer : row_buffer_array;
signal reg, next_reg : halfword_t := halfword_zero;
signal state_t2, next_state_t2 : state_type_t2 := idle;
signal x_position, y_position : integer := 0;
signal top_line_left, top_line_right : word_t := word_zero;

begin
    task2 : process(start, state_t2)
    begin
        next_state_t2 <= state_t2;
        next_reg <= reg;
        addr <= reg;

        case (next_state_t2) is
            when idle =>
                if start = '1' then 
                    en <= '1'; 
                    state_t2 <= read;                   
                end if;
            when read =>
                -- check if you read three rows
                if y_position mod 3 = 0 then
                    state_t2 <= computation;
                else
                    y_position = y_position + 1;
                    reg <= reg + (y_position * 352);
                end if;
            when computation => 
        end case;
    end process task2;

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
    
    -- task0
    -- cl : process(start,state)
    -- begin
    --     next_state <= state;
    --     next_reg <= reg;
    --     finish <= '0';
    --     addr <= reg;
    --     case (state) is
    --         when idle =>
    --             if start = '1' then
    --                 en <= '1';
    --                 next_state <= invert;
    --             end if;
    --         when invert =>
    --             dataW <= not(dataR);
    --             next_reg <= std_logic_vector(unsigned(reg) + 25344);
    --             next_state <= write;
    --         when write =>
    --             we <= '1';
    --             next_state <= check_end;
    --         when check_end =>
    --         -- 25343 means this was the last word to read
    --             if (to_integer(unsigned(reg)) - 25344) /= 25343 then
    --                 we <= '0';  
    --                 next_reg <= std_logic_vector(unsigned(reg) - 25343);
    --             else
    --                 we <= '0';
    --                 en <= '0';
    --                 finish <= '1';
    --             end if;
    --             next_state<= idle;
    --     end case;    
    -- end process cl;
end rtl;
