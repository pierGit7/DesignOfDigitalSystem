-- -----------------------------------------------------------------------------
--
--  Title      :  Edge-Detection design project - task 2. 😎
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
 
    type state_type is (idle, read, computation, write, done, initialise, load, bram_read);
 
    type computation_state is (first_computation, second_computation, no_computation, EOL_computation);
 
    -- Buffer of three rows
    type row_buffer_array is array(0 to 87) of word_t;
    --type computation_register is std_logic_vector(0 downto 47);
 
    -- buffer three rows to acces three words each clock cycle
    signal row1_buffer, row2_buffer, row3_buffer : row_buffer_array := (others=>(others => '0'));
 
    signal row1_read_data, row2_read_data, row3_read_data : word_t;
    signal row1_read_addr, row2_read_addr, row3_read_addr : integer range 0 to 87 := 0;
 
    signal row1_write_enable, row2_write_enable, row3_write_enable : std_logic := '0';
    signal row_write_index : integer range 0 to 87 := 0;
    signal row_write_data : word_t := (others => '0');
 
    attribute ram_style : string;
    attribute ram_style of row1_buffer : signal is "block";
    attribute ram_style of row2_buffer : signal is "block";
    attribute ram_style of row3_buffer : signal is "block";
 
    -- computation registers
    signal comp1, comp2, comp3, next_comp1, next_comp2, next_comp3 : std_logic_vector(47 downto 0) := (others => '0');
 
    -- computation state
    signal comp_state, next_comp_state : computation_state := no_computation;
 
    -- changing address for main memory
    signal reg, next_reg, read_reg, next_read_reg : halfword_t := halfword_zero;
    signal next_write_reg, write_reg : halfword_t := std_logic_vector(to_unsigned(25432, 16));
 
    --state of task2 process
    signal state, next_state : state_type := idle;
 
    -- Start from pixel 1
    signal x_position, next_x_position : integer := 0;
 
    signal index_of_buffer, next_index_of_buffer : integer := 0;
    -- init to 1 because the first address is loadded during the init phase
    signal index_of_load, next_index_of_load : integer := 2;
 
    signal next_index_of_computation, index_of_computation : integer := 0;
 
    signal test_flag : boolean := false; -- Testing if we are reading correctly
 
    signal pixel_store, next_pixel_store : std_logic_vector(7 downto 0) := (others => '0');
 
    signal pix1_x, pix1_y, pix2_x, pix2_y : signed(10 downto 0) := (others => '0');
    signal pix1, pix2 : integer := 0;
 
    signal prev_dataW, next_dataW : word_t := (others => '0');
 
    signal halt : boolean := false;
 
begin
    -- row1_process : process(clk)
    -- begin
    --     if rising_edge(clk) then
    --         if row1_write_enable = '1' then
    --             row1_buffer(row_write_index) <= row_write_data;
    --         end if;
    --         row1_read_data <= row1_buffer(row_write_index);
    --     end if;
    -- end process;
 
    -- row2_process : process(clk)
    -- begin
    --     if rising_edge(clk) then
    --         if row2_write_enable = '1' then
    --             row2_buffer(row_write_index) <= row_write_data;
    --         end if;
    --         row2_read_data <= row2_buffer(row_write_index);
    --     end if;
    -- end process;
 
    -- row3_process : process(clk)
    -- begin
    --     if rising_edge(clk) then
    --         if row3_write_enable = '1' then
    --             row3_buffer(row_write_index) <= row_write_data;
    --         end if;
    --         row3_read_data <= row3_buffer(row_write_index);
    --     end if;
    -- end process;
 
    task2 : process(start, state, x_position, dataR, write_reg, reg, read_reg, comp1, comp2, comp3, index_of_buffer, index_of_computation, index_of_load, next_reg, comp_state, row1_read_data, row2_read_data, row3_read_data)
    begin
        -- Default assignments to prevent latches
        finish <= '0';
        we <= '0';
        en <= '0';
        next_state <= state;
        next_write_reg <= write_reg;
        next_reg <= reg;
        next_read_reg <= read_reg;
 
        --next_read1 <= read1;
        --next_read2 <= read2;
        --next_read3 <= read3;
 
        next_comp1 <= comp1;
        next_comp2 <= comp2;
        next_comp3 <= comp3;
 
        next_comp_state <= comp_state;
 
        next_x_position <= x_position;
 
        next_index_of_buffer <= index_of_buffer;
        next_index_of_computation <= index_of_computation;
        next_index_of_load <= index_of_load;
 
        row1_write_enable <= '0';
        row2_write_enable <= '0';
        row3_write_enable <= '0';
        
        row_write_index <= x_position;
        row_write_data <= dataR;
 
        case (state) is
            when idle =>
                if start = '1' then 
                    we <= '0';
                    en <= '1'; 
                    next_state <= initialise; 
                    next_reg <=   std_logic_vector(unsigned(reg) + 1);                
                end if;
            when initialise =>
                --keep reading state
                we <= '0';
                en <= '1'; 
 
                -- fill the correct buffer
                if index_of_buffer = 0 then 
                    row1_write_enable <= '1';
                elsif index_of_buffer = 1 then
                    row2_write_enable <= '1';
                elsif index_of_buffer = 2 then
                    row3_write_enable <= '1';
                end if;
 
 
                -- see if you have read the first 3 addresses of the third row and start writing
                --otherwise go to the next addresses and x_position
                if index_of_buffer = 2 then
 
                    if x_position = 0 then
                        next_state <= bram_read;
                    end if;
 
                    if x_position = 1 then
                        next_comp1(31 downto 0) <=  row1_read_data;
                        next_comp2(31 downto 0) <=  row2_read_data;
                        next_comp3(31 downto 0) <=  row3_read_data;
 
                        next_index_of_computation <= index_of_computation + 1;
 
                        next_comp_state <= first_computation;             
                    end if;
 
                    if x_position = 2 then
                        we <= '0';
                        en <= '0';
 
                        -- load the next computation
                        next_state <= bram_read;
 
                        --next_reg <= write_reg;
                        next_x_position <= x_position + 1;
 
                        --next_comp_state <= first_computation;
                    else
 
                        if x_position = 87 then 
                            next_index_of_buffer <= index_of_buffer + 1;
                            next_x_position <= 0;
 
                        else
                            next_x_position <= x_position + 1;
                        end if;
                        next_reg <= std_logic_vector(unsigned(reg) + 1);
 
                    end if;
                else
 
                    if x_position = 87 then 
                        next_index_of_buffer <= index_of_buffer + 1;
                        next_x_position <= 0;
 
                    else
                        next_x_position <= x_position + 1;
                    end if;
                    next_reg <= std_logic_vector(unsigned(reg) + 1);
 
                end if;
 
                -- take the last read registers address
                next_read_reg <= next_reg;
            when bram_read =>
                row3_write_enable <= '0';
                if to_integer(unsigned(read_reg)) < 179 then
                    next_state <= initialise;
                else
                    next_state <= load;
                end if;
 
            when write =>
                --do something
 
                -- writing flag
                we <= '1';
                en <= '1';
 
                --increment the address
                next_write_reg <= std_logic_vector(unsigned(write_reg) + 1);
                next_reg <= read_reg;
 
                --  shift the computation registers to the right and keep just the last byte 
                next_comp1 <= std_logic_vector(shift_right(unsigned(comp1), 16));
                next_comp2 <= std_logic_vector(shift_right(unsigned(comp2), 16));
                next_comp3 <= std_logic_vector(shift_right(unsigned(comp3), 16));
 
 
 
                --next state
                next_comp_state <= first_computation;        
                next_state <= load;
 
                if unsigned(write_reg) = 50600 then 
                    next_state <= done;
                    finish <= '1';
                end if;
            when load =>
 
                -- Checks which computation reg needs which pixels
                -- how do you increment the index ? the index represent wich position you are computing
 
                                -- first or second computation ?
 
                next_comp_state <= second_computation;
 
                next_comp1 <= std_logic_vector(shift_right(unsigned(comp1), 16));
                next_comp2 <= std_logic_vector(shift_right(unsigned(comp2), 16));
                next_comp3 <= std_logic_vector(shift_right(unsigned(comp3), 16));
 
                if index_of_load = 2 then                                
                    next_comp1(47 downto 16) <= row1_read_data;
					next_comp2(47 downto 16) <= row2_read_data;
					next_comp3(47 downto 16) <= row3_read_data;
				elsif index_of_load = 0 then
					next_comp1(47 downto 16) <= row2_read_data;
					next_comp2(47 downto 16) <= row3_read_data;
					next_comp3(47 downto 16) <= row1_read_data;
				else 
                    next_comp1(47 downto 16) <= row3_read_data;
					next_comp2(47 downto 16) <= row1_read_data;
					next_comp3(47 downto 16) <= row2_read_data;
				end if;
 
                next_index_of_computation <= index_of_computation + 1;
 
                -- check if you finish this row
                if index_of_computation = 87 then
                    next_index_of_computation <= 0;
                    if index_of_load = 2 then
                        next_index_of_load <= 0;
                    else
                        next_index_of_load <= index_of_load + 1;
                    end if;
                elsif index_of_computation = 0 then
                    next_comp_state <= EOL_computation;  
                    next_index_of_computation <= index_of_computation + 1;                    
                else
                    next_index_of_computation <= index_of_computation + 1;
                end if;
 
 
                --prepare for read
                we <= '0';
                en <= '1';
                next_state <= read;
                next_reg <= read_reg;
                --load the computation accordingly and start the new computation
 
            when read =>
 
                we <= '0';
                en <= '1';
 
                --  shift the computation registers to the right and keep just the last byte 
                --next_comp1 <= std_logic_vector(shift_right(unsigned(comp1), 16));
                --next_comp2 <= std_logic_vector(shift_right(unsigned(comp2), 16));
                --next_comp3 <= std_logic_vector(shift_right(unsigned(comp3), 16));
 
                -- fill the correct buffer
                if index_of_buffer = 0 then 
                    row1_write_enable <= '1';
                elsif index_of_buffer = 1 then
                    row2_write_enable <= '1';
                elsif index_of_buffer = 2 then
                    row3_write_enable <= '1';
                end if;
 
 
                -- increment the index of buffer
                if x_position = 87 then 
                    if index_of_buffer = 2 then
                        next_index_of_buffer <= 0;
                    else
                        next_index_of_buffer <= index_of_buffer + 1;
                    end if;
                    next_x_position <= 0;
 
                else
                    next_x_position <= x_position + 1;
 
                end if;
 
                --next state
                next_read_reg <= std_logic_vector(unsigned(read_reg) + 1);
                next_reg <= write_reg;
                next_state <= write;
                next_comp_state <= no_computation; -- Skip a computation
 
 
            when done =>
                finish <= '1';
                if start = '1' then
                    next_state <= done;
                else
                    next_state <= idle;
                end if;
            when others =>
                next_state <= idle;
        end case;
    end process task2;
 
    pix1_x <= resize(to_signed(to_integer(unsigned(comp1(23 downto 16))), 9), 11) - resize(to_signed(to_integer(unsigned(comp1(7 downto 0))), 9), 11) + resize(2*to_signed(to_integer(unsigned(comp2(23 downto 16))), 9), 11) - resize(2*to_signed(to_integer(unsigned(comp2(7 downto 0))), 9), 11) + resize(to_signed(to_integer(unsigned(comp3(23 downto 16))), 9), 11) - resize(to_signed(to_integer(unsigned(comp3(7 downto 0))), 9), 11);
    pix1_y <= resize(to_signed(to_integer(unsigned(comp1(7 downto 0))), 9), 11) - resize(to_signed(to_integer(unsigned(comp3(7 downto 0))), 9), 11) + resize(2*to_signed(to_integer(unsigned(comp1(15 downto 8))), 9), 11) - resize(2*to_signed(to_integer(unsigned(comp3(15 downto 8))), 9), 11) + resize(to_signed(to_integer(unsigned(comp1(23 downto 16))), 9), 11) - resize(to_signed(to_integer(unsigned(comp3(23 downto 16))), 9), 11);
 
    pix2_x <= resize(to_signed(to_integer(unsigned(comp1(31 downto 24))), 9), 11) - resize(to_signed(to_integer(unsigned(comp1(15 downto 8))), 9), 11) + resize(2*to_signed(to_integer(unsigned(comp2(31 downto 24))), 9), 11) - resize(2*to_signed(to_integer(unsigned(comp2(15 downto 8))), 9), 11) + resize(to_signed(to_integer(unsigned(comp3(31 downto 24))), 9), 11) - resize(to_signed(to_integer(unsigned(comp3(15 downto 8))), 9), 11);
    pix2_y <= resize(to_signed(to_integer(unsigned(comp1(15 downto 8))), 9), 11) - resize(to_signed(to_integer(unsigned(comp3(15 downto 8))), 9), 11) + resize(2*to_signed(to_integer(unsigned(comp1(23 downto 16))), 9), 11) - resize(2*to_signed(to_integer(unsigned(comp3(23 downto 16))), 9), 11) + resize(to_signed(to_integer(unsigned(comp1(31 downto 24))), 9), 11) - resize(to_signed(to_integer(unsigned(comp3(31 downto 24))), 9), 11);
 
    pix1 <= to_integer(abs(pix1_x) + abs(pix1_y));
    pix2 <= to_integer(abs(pix2_x) + abs(pix2_y));
 
    compute_process : process(comp_state, pixel_store, prev_dataW, pix1, pix2)
    begin
        next_pixel_store <= pixel_store;
        next_dataW <= prev_dataW;
        dataW <= (others => '0');
        case(comp_state) is 
            when first_computation =>
                next_dataW(7 downto 0) <= pixel_store;
                -- If final value is greater than 255, clip to 255
                if pix1 > 255 then
                    next_dataW(15 downto 8) <= (others => '1');
                else
                    next_dataW(15 downto 8) <= std_logic_vector(to_unsigned(pix1, 8));
                end if; 
                -- If final value is greater than 255, clip to 255
                if pix2 > 255 then
                    next_dataW(23 downto 16) <= (others => '1');
                else
                    next_dataW(23 downto 16) <= std_logic_vector(to_unsigned(pix2, 8));
                end if;
                -- Shifting Comp registers to place next two pixels to be computed at the same place 
            when second_computation =>       
 
                -- Sets the real pixel register for writing
                -- If final value is greater than 255, clip to 255
 
                --next_dataW(23 downto 0) <= prev_dataW(23 downto 0);
                next_dataW(23 downto 0) <= prev_dataW(23 downto 0);
                if pix1 > 255 then
                    next_dataW(31 downto 24) <= (others => '1');
                else
                    next_dataW(31 downto 24) <= std_logic_vector(to_unsigned(pix1, 8));
                end if;
                -- Store last pixel for next address
                -- If final value is greater than 255, clip to 255
                if pix2 > 255 then
                    next_pixel_store <= (others => '1');
                else
                    next_pixel_store <= std_logic_vector(to_signed(pix2, 8));
                end if;
 
 
            when no_computation =>
                dataW <= prev_dataW;
            when EOL_computation =>
                next_dataW(31 downto 24) <= (others => '0');
                next_pixel_store <= (others => '0');
            when others =>
               null;
        end case;
    end process compute_process;
 
 
 
 
    -- Register process
    register_process : process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                addr <= (others => '0');
                state <= idle;
                comp_state <= no_computation;
 
                reg <= (others => '0');
                write_reg <= std_logic_vector(to_unsigned(25432, 16));
                read_reg <= (others => '0');
 
                index_of_buffer <= 0;
                index_of_load <= 2;
                index_of_computation <= 0;
 
                row1_read_addr <= 0;
                row2_read_addr <= 0;
                row3_read_addr <= 0;
 
                --row1_write_enable <= '0';
                --row2_write_enable <= '0';
                --row3_write_enable <= '0';
 
                --row_write_index <= 0;
 
                comp1 <= (others => '0');
                comp2 <= (others => '0');
                comp3 <= (others => '0');
 
                pixel_store <= (others => '0');
 
                x_position <= 0;
            else
                -- Registers update
                state <= next_state;
 
                reg <= next_reg;
                write_reg <= next_write_reg;
                read_reg <= next_read_reg;
                addr <= next_reg;
 
                --read1 <= next_read1;
                --read2 <= next_read2;
                --read3 <= next_read3;
 
                row1_read_addr <= index_of_computation;
                row2_read_addr <= index_of_computation;
                row3_read_addr <= index_of_computation;
 
                prev_dataW <= next_dataW;
 
                x_position <= next_x_position;
                pixel_store <= next_pixel_store;
                comp_state <= next_comp_state;
 
                comp1 <= next_comp1;
                comp2 <= next_comp2;
                comp3 <= next_comp3;
 
                index_of_buffer <= next_index_of_buffer;
                index_of_computation <= next_index_of_computation;
                index_of_load <= next_index_of_load;
 
                -- Fill first buffer if enabled
                if row1_write_enable = '1' then
                    row1_buffer(row_write_index) <= row_write_data;
                else
                    row1_read_data <= row1_buffer(row1_read_addr);
                end if;
 
 
                -- Fill second buffer if enabled      
                if row2_write_enable = '1' then
                    row2_buffer(row_write_index) <= row_write_data;
                else
                    row2_read_data <= row2_buffer(row2_read_addr);
                end if;
 
 
                -- Fill third buffer if enabled
                if row3_write_enable = '1' then
                    row3_buffer(row_write_index) <= row_write_data;
                else
                    row3_read_data <= row3_buffer(row3_read_addr);
                end if;
 
            end if;
        end if;
    end process register_process;
 
end rtl;