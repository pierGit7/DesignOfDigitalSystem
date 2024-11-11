-- -----------------------------------------------------------------------------
--
--  Title      :  Edge-Detection design project - task 2.
--             :
--  Developers :  YOUR NAME HERE - s??????@student.dtu.dk
--             :  YOUR NAME HERE - s??????@student.dtu.dk
--             :
--  Purpose    :  This design contains an entity for the accelerator that must be built
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
-- The description of the accelerator.
--------------------------------------------------------------------------------

architecture rtl of acc is

    -- All internal signals are defined here

    type state_type_t2 is (idle, read, computation);

    -- Buffer of three rows
    type row_buffer_array is array(0 to 87) of word_t;
    --type computation_register is std_logic_vector(0 downto 47);

    --TODO: init variables to 0
    -- buffer three rows to acces three words each clock cycle
    signal row1_buffer, row2_buffer, row3_buffer : row_buffer_array;

    signal comp1, comp2, comp3 : std_logic_vector(0 downto 47);
    --TODO alias
    
    -- changing addres for main memory
    signal next_addr : halfword_t := halfword_zero;

    --state of task2 process
    signal state_t2, next_state_t2 : state_type_t2 := idle;

    signal y_position : integer := 0;  -- Explicit width
    -- Start from pixel 1
    signal x_position : integer := 0;

    signal index_of_buffer : integer := 0;

begin
    task2 : process(start, state_t2)
    begin
        -- Default assignments to prevent latches
        finish <= '0';
        next_state_t2 <= state_t2;
        next_addr <= addr;


        case (state_t2) is
            when idle =>
                if start = '1' then 
                    en <= '1'; 
                    next_state_t2 <= read;                   
                end if;

            when read =>
                x_position <= x_position + 1;
                next_addr <=  addr + 1;
                -- check wich row are we in
                if x_position = 87  then
                    index_of_buffer <= index_of_buffer + 1;
                    if index_of_buffer = 2 then
                        index_of_buffer <= 0;
                    end if;
                    y_position <= y_position + 1;
                    x_position <= 0;
                end if; 
                if index_of_buffer = 0 then
                    row1_buffer(x_position) <= dataR;
                elsif index_of_buffer = 1 then
                    row2_buffer(x_position) <= dataR;
                else
                    row3_buffer(x_position) <= dataR;
                end if;
                next_reg <= std_logic_vector(unsigned(reg) + 1);
            when computation =>
                finish <= '1';  -- Signal the completion
                next_state_t2 <= idle;  -- Go back to idle after computation is done
            when others =>
                next_state_t2 <= idle;
        end case;
    end process task2;
    
    

    -- Register process
    register_process : process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                addr <= (others => '0');
                state_t2 <= idle;
                reg <= (others => '0');
                y_position <= 0;  -- Initialize y_position
                --en <= '0';
                --we <= '0';
            else
                -- Registers update
                state_t2 <= next_state_t2;
                reg <= next_reg;
                addr <= next_addr;
            end if;
        end if;
    end process register_process;

end rtl;
