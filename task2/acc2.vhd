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

    type state_type is (idle, read, computation);

    -- Buffer of three rows
    type row_buffer_array is array(87 to 0) of word_t;
    --type computation_register is std_logic_vector(0 downto 47);

    --TODO: init variables to 0
    -- buffer three rows to acces three words each clock cycle
    signal row1_buffer, row2_buffer, row3_buffer : row_buffer_array;

    signal comp1, comp2, comp3 : std_logic_vector(47 downto 0);
    --TODO alias
    
    -- changing addres for main memory
    signal reg, next_reg : halfword_t := halfword_zero;

    --state of task2 process
    signal state, next_state : state_type := idle;

    signal y_position : integer := 0;  -- Explicit width
    -- Start from pixel 1
    signal x_position : integer := 0;

    signal index_of_buffer : integer := 0;
    
    signal pixel_out : std_logic_vector(39 downto 0);
    
    signal output_flag : boolean := false;
    
    signal Dx_1, Dy_1, Dx_2, Dy_2 : signed(63 downto 0);

begin
    task2 : process(start, state)
    begin
        -- Default assignments to prevent latches
        finish <= '0';
        next_state_t2 <= state_t2;
        next_reg <= reg;
        addr <= reg;


        case (state_t2) is
            when idle =>
                if start = '1' then 
                    en <= '1'; 
                    next_state_t2 <= read;                   
                end if;

            when read =>
                x_position <= x_position + 1;
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
                Dx_1 <= signed(unsigned(comp1(23 downto 16)) - unsigned(comp1(7 downto 0)) + 2*(unsigned(comp2(23 downto 16)) - unsigned(comp2(7 downto 0))) + unsigned(comp3(23 downto 16)) - unsigned(comp3(7 downto 0)));
                Dy_1 <= signed(unsigned(comp1(7 downto 0)) - unsigned(comp3(7 downto 0)) + 2*(unsigned(comp1(15 downto 8)) - unsigned(comp3(15 downto 8))) + unsigned(comp1(23 downto 16)) - unsigned(comp3(23 downto 16)));
                
                Dx_2 <= signed(unsigned(comp1(31 downto 24)) - unsigned(comp1(7 downto 0)) + 2*(unsigned(comp2(23 downto 16)) - unsigned(comp2(7 downto 0))) + unsigned(comp3(23 downto 16)) - unsigned(comp3(7 downto 0)));
                Dx_2 <= ;
                
                if output_flag = false then
                    pixel_out(15 downto 8) <= std_logic_vector(abs(Dx_1) + abs(Dy_1));
                    pixel_out(23 downto 16) <= std_logic_vector(abs(Dx_2) + abs(Dy_2));
                    output_flag <= true;
                else
                    pixel_out(31 downto 24) <= std_logic_vector(abs(Dx_1) + abs(Dy_1));
                    pixel_out(39 downto 32) <= std_logic_vector(abs(Dx_2) + abs(Dy_2));
                    output_flag <= false;
                    next_state <= write;
                end if;


                next_reg <= std_logic_vector(unsigned(reg) + 25343);
                
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
                addr <= next_reg;
            end if;
        end if;
    end process register_process;

end rtl;
