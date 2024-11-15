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

    type state_type is (idle, read, computation, write, check_finish, initialise);

    -- Buffer of three rows
    type row_buffer_array is array(0 to 87) of word_t;
    --type computation_register is std_logic_vector(0 downto 47);

    --TODO: init variables to 0
    -- buffer three rows to acces three words each clock cycle
    signal row1_buffer, row2_buffer, row3_buffer : row_buffer_array := (others=>(others => '0'));

    -- computation registers
    signal comp1, comp2, comp3 : std_logic_vector(47 downto 0);
    --TODO alias
    
    -- changing addres for main memory
    signal reg, next_reg, write_reg, next_write_reg, read_reg, next_read_reg : halfword_t := halfword_zero;

    --state of task2 process
    signal state, next_state : state_type := idle;

    signal y_position, next_y_position : integer := 0;  -- Explicit width
    -- Start from pixel 1
    signal x_position, next_x_position : integer := 0;

    signal index_of_buffer, index_of_load, next_index_of_load : integer := 0;
    
    signal output_flag : boolean := true;
    
    signal EOL_flag : boolean := false;
    
    signal pixel_out, next_pixel_out, pixel_temp : std_logic_vector(39 downto 0);

begin
    task2 : process(start, state)
    begin
        -- Default assignments to prevent latches
        finish <= '0';
        next_state <= state;
        --addr <= next_reg;
        next_reg <= reg;
        case (state) is
            when idle =>
                if start = '1' then 
                    we <= '0';
                    en <= '1'; 
                    next_state <= initialise;                   
                end if;
			when initialise => -- Read the first and second lines into Buffers then begin computing the first 4 pixels
				next_x_position <= x_position + 1; 
				next_state <= initialise;
                -- check wich row are we in
                EOL_flag <= false;
                if x_position = 87  then -- Check end of line
                    EOL_flag <= true;    -- Set end of line flag for computation (the computation function should not run for the last and first pixels of a line)
                    index_of_buffer <= index_of_buffer + 1;
                    if index_of_buffer = 2 then
                        index_of_buffer <= 0;
                    end if;
                    next_y_position <= y_position + 1;
                    next_x_position <= 0;
                end if;
                if index_of_buffer = 0 then
                    row1_buffer(x_position) <= dataR; -- Read to first buffer
                elsif index_of_buffer = 1 then
                    row2_buffer(x_position) <= dataR; -- Read to second buffer
				else 
					row3_buffer(x_position) <= dataR; -- Read to third buffer
					if index_of_load = 0 then 										-- load first 4 pixels into computation buffers
						comp1(47 downto 16) <= row1_buffer(index_of_load);
						comp2(47 downto 16) <= row2_buffer(index_of_load);
						comp3(47 downto 16) <= row2_buffer(index_of_load);
						next_index_of_load <= index_of_load + 1;
						output_flag <= false; 									    -- Begin first computation 
					elsif index_of_load = 1 then 									-- Shift 2 pixels out and load second 4 pixels into computation buffers
					    comp1 <= std_logic_vector(shift_left(unsigned(comp1), 16));
                        comp2 <= std_logic_vector(shift_left(unsigned(comp2), 16));
                        comp3 <= std_logic_vector(shift_left(unsigned(comp3), 16));
						comp1(31 downto 0) <= row1_buffer(index_of_load);
						comp2(31 downto 0) <= row2_buffer(index_of_load);
						comp3(31 downto 0) <= row3_buffer(index_of_load);
						next_index_of_load <= index_of_load + 1;
						output_flag <= true; 									    -- Begin second computation
						next_state <= read;
				    end if;
                end if;
            when read =>
                next_x_position <= x_position + 1;
				next_reg <= write_reg;                                              -- Keeping track of which write address we are in
				EOL_flag <= false;
                -- check wich row are we in
                if x_position = 87  then
                    EOL_flag <= true;
                    index_of_buffer <= index_of_buffer + 1;
                    if index_of_buffer = 2 then
                        index_of_buffer <= 0;
                    end if;
                    next_y_position <= y_position + 1;
                    next_x_position <= 0;
                else
                    output_flag <= false;	--Begin Computation
					-- Shifting Comp registers to place next two pixels to be computed at the same place
                    comp1 <= std_logic_vector(shift_left(unsigned(comp1), 16));
                    comp2 <= std_logic_vector(shift_left(unsigned(comp2), 16));
                    comp3 <= std_logic_vector(shift_left(unsigned(comp3), 16));
                    
				end if;
				if NOT((x_position >= 87) AND (y_position >= 287)) then --Check if we are at the end of the image
                    if index_of_buffer = 0 then
                        row1_buffer(x_position) <= dataR;
                    elsif index_of_buffer = 1 then
                        row2_buffer(x_position) <= dataR;
                    else
                        row3_buffer(x_position) <= dataR;
                    end if;
                end if;
				we <= '1';
                next_state <= write;				
                next_read_reg <= std_logic_vector(unsigned(read_reg) + 1); -- Sets the write register
				
            when write =>
				next_reg <= read_reg;               --Sets the addr to the current read addr
                dataW <= pixel_out(39 downto 8);    --Writes the current pixel buffer to memory
                we <= '0';
                next_write_reg <= std_logic_vector(unsigned(write_reg) + 1); --increments the writing addr
				next_state <= read;
				-- Shifting Comp registers to place next two pixels to be computed at the same place
                comp1 <= std_logic_vector(shift_left(unsigned(comp1), 16));
                comp2 <= std_logic_vector(shift_left(unsigned(comp2), 16));
                comp3 <= std_logic_vector(shift_left(unsigned(comp3), 16));
                next_index_of_load <= index_of_load + 1;                     -- increments the loading index
				if index_of_buffer = 2 then                                  -- Checks which computation buffer needs which pixels
					comp1(31 downto 0) <= row1_buffer(index_of_load);
					comp2(31 downto 0) <= row2_buffer(index_of_load);
					comp3(31 downto 0) <= row3_buffer(index_of_load);
					
				elsif index_of_buffer = 0 then
					comp1(31 downto 0) <= row2_buffer(index_of_load);
					comp2(31 downto 0) <= row3_buffer(index_of_load);
					comp3(31 downto 0) <= row1_buffer(index_of_load);
				else 
					comp1(31 downto 0) <= row3_buffer(index_of_load);
					comp2(31 downto 0) <= row1_buffer(index_of_load);
					comp3(31 downto 0) <= row2_buffer(index_of_load);
				end if;

				output_flag <= true;	--Begin Computation
				
				if unsigned(write_reg) = 50335 then     --Check if write is at the end of the image
				    finish <= '1';
				    next_state <= idle;
				end if;
            when others =>
                next_state <= idle;
        end case;
    end process task2;
    
    compute_process : process(output_flag)
    begin
        if EOL_flag = false then
            if output_flag = false then
                pixel_temp <= std_logic_vector(shift_left(unsigned(pixel_out), 32));
                --Sets the pixel temp register
                pixel_temp(31 downto 24) <= std_logic_vector(abs(signed(unsigned(comp1(31 downto 24)) - unsigned(comp1(47 downto 40)) + 2*(unsigned(comp2(31 downto 24)) - unsigned(comp2(47 downto 40))) + unsigned(comp3(31 downto 24)) - unsigned(comp3(47 downto 40)))) + abs(signed(unsigned(comp1(47 downto 40)) - unsigned(comp3(47 downto 40)) + 2*(unsigned(comp1(39 downto 32)) - unsigned(comp3(39 downto 32))) + unsigned(comp1(31 downto 24)) - unsigned(comp3(31 downto 24)))));
                pixel_temp(23 downto 16) <= std_logic_vector(abs(signed(unsigned(comp1(23 downto 16)) - unsigned(comp1(39 downto 32)) + 2*(unsigned(comp2(23 downto 16)) - unsigned(comp2(39 downto 32))) + unsigned(comp3(23 downto 16)) - unsigned(comp3(39 downto 32)))) + abs(signed(unsigned(comp1(39 downto 32)) - unsigned(comp3(39 downto 32)) + 2*(unsigned(comp1(31 downto 24)) - unsigned(comp3(31 downto 24))) + unsigned(comp1(23 downto 16)) - unsigned(comp3(23 downto 16)))));
               -- Shifting Comp registers to place next two pixels to be computed at the same place 
            else
                next_pixel_out <= pixel_temp;
                --Sets the real pixel register for writing
               next_pixel_out(31 downto 24) <= std_logic_vector(abs(signed(unsigned(comp1(31 downto 24)) - unsigned(comp1(47 downto 40)) + 2*(unsigned(comp2(31 downto 24)) - unsigned(comp2(47 downto 40))) + unsigned(comp3(31 downto 24)) - unsigned(comp3(47 downto 40)))) + abs(signed(unsigned(comp1(47 downto 40)) - unsigned(comp3(47 downto 40)) + 2*(unsigned(comp1(39 downto 32)) - unsigned(comp3(39 downto 32))) + unsigned(comp1(31 downto 24)) - unsigned(comp3(31 downto 24)))));
               next_pixel_out(39 downto 32) <= std_logic_vector(abs(signed(unsigned(comp1(23 downto 16)) - unsigned(comp1(39 downto 32)) + 2*(unsigned(comp2(23 downto 16)) - unsigned(comp2(39 downto 32))) + unsigned(comp3(23 downto 16)) - unsigned(comp3(39 downto 32)))) + abs(signed(unsigned(comp1(39 downto 32)) - unsigned(comp3(39 downto 32)) + 2*(unsigned(comp1(31 downto 24)) - unsigned(comp3(31 downto 24))) + unsigned(comp1(23 downto 16)) - unsigned(comp3(23 downto 16)))));
                
            end if;
         end if;
    end process compute_process;

    -- Register process
    register_process : process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                addr <= (others => '0');
                state <= idle;
                reg <= (others => '0');
                y_position <= 0; 
            else
                -- Registers update
                state <= next_state;
                reg <= next_reg;
                write_reg <= next_write_reg;
                read_reg <= next_read_reg;
                index_of_load <= next_index_of_load;
                addr <= next_reg;
                y_position <= next_y_position;
                x_position <= next_x_position;
				--prev_x_position <= x_position;
            end if;
        end if;
    end process register_process;

end rtl;