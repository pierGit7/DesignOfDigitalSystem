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

    type state_type_t2 is (idle, read, computation, write, check_finish);

    -- Buffer of three rows
    type row_buffer_array is array(87 to 0) of word_t;
    --type computation_register is std_logic_vector(0 downto 47);

    --TODO: init variables to 0
    -- buffer three rows to acces three words each clock cycle
    signal row1_buffer, row2_buffer, row3_buffer : row_buffer_array;

    signal comp1, comp2, comp3 : std_logic_vector(47 downto 0);
    --TODO alias
    
    -- changing addres for main memory
    signal reg, next_reg, write_reg, next_write_reg : halfword_t := halfword_zero;

    --state of task2 process
    signal state, next_state : state_type_t2 := idle;

    signal y_position, next_y_position : integer := 0;  -- Explicit width
    -- Start from pixel 1
    signal x_position, next_x_position : integer := 0;

    signal index_of_buffer, next_index_of_buffer : integer := 0;
    
    signal pixel_out : std_logic_vector(7 downto 0);

begin
    task2 : process(start, state)
    begin
        -- Default assignments to prevent latches
        finish <= '0';
        next_state <= state;
        next_reg <= reg;
        next_write_reg <= write_reg;
        next_y_position <= y_position;
        next_x_position <= x_position;
        next_index_of_buffer <= index_of_buffer;
        addr <= reg;


        case (state) is
            when idle =>
                if start = '1' then 
                    en <= '1'; 
                    next_state <= read;                   
                end if;

            when read =>
                next_x_position <= x_position + 1;
                -- check wich row are we in
                if x_position = 87  then
                    next_index_of_buffer <= index_of_buffer + 1;
                    if index_of_buffer = 2 then
                        next_index_of_buffer <= 0;
                    end if;
                    next_y_position <= y_position + 1;
                    next_x_position <= 0;
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
                next_state <= write;  -- Go back to idle after computation is done
            when write =>
                dataW <= pixel_out;
                we <= '0';
                next_state <= check_finish;
                next_reg <= std_logic_vector(unsigned(reg) - 25343);
            when check_finish =>
                next_state <= idle;
                
            when others =>
                next_state <= idle;
        end case;
    end process task2;
    
    

    -- Register process
    register_process : process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                addr <= (others => '0');
                state <= idle;
                reg <= (others => '0');
                y_position <= next_y_position;  -- Initialize y_position
                x_position <= next_x_position;  -- Initialize x_position
                index_of_buffer <= next_index_of_buffer;
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
