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

    type state_type is (idle, read, computation, write, check_finish, initialise, load);

    type computation_state is (first_computation, second_computation, no_computation, EOL_computation);

    -- Buffer of three rows
    type row_buffer_array is array(0 to 87) of word_t;
    --type computation_register is std_logic_vector(0 downto 47);

    -- buffer three rows to acces three words each clock cycle
    signal row1_buffer, row2_buffer, row3_buffer : row_buffer_array := (others=>(others => '0'));

    -- computation registers
    signal comp1, comp2, comp3 : std_logic_vector(47 downto 0) := (others => '0');
    
    -- computation state
    signal comp_state, next_comp_state : computation_state := no_computation;
    
    -- changing address for main memory
    signal reg, next_reg, read_reg, next_read_reg : halfword_t := halfword_zero;
    signal next_write_reg, write_reg : halfword_t := std_logic_vector(to_unsigned(25432, 16));

    --state of task2 process
    signal state, next_state : state_type := idle;

    signal y_position, next_y_position : integer := 0;  -- Explicit width
    -- Start from pixel 1
    signal x_position, next_x_position : integer := 0;
    
    signal index_of_buffer: integer := 0;
    -- init to 1 because the first address is loadded during the init phase
    signal index_of_computation : integer := 1;
    
    signal output_flag : boolean := true;
    
    signal EOL_flag, EOB_flag : boolean := false;
    
    signal test_flag : boolean := false; -- Testing if we are reading correctly
    
    signal pixel_out, next_pixel_out, pixel_temp : std_logic_vector(39 downto 0) := (others => '0');
    
    
    --signal pixel_out, next_pixel_out, pixel_temp, next_pixel_temp : std_logic_vector(39 downto 0) := (others => '0');
    signal pixel_store, next_pixel_store : std_logic_vector(7 downto 0) := (others => '0');
 
    signal p1_1, p2_1, p3_1, p4_1, p1_2, p2_2, p3_2, p4_2 : signed(10 downto 0) := (others => '0');
    signal Dx_pix1_sig, Dy_pix1_sig, Dx_pix2_sig, Dy_pix2_sig : signed(10 downto 0) := (others => '0');
    --signal pix1, pix2 : signed(11 downto 0) := (others => '0');
    signal pix1_sig, pix2_sig : integer := 0; 

begin
    task2 : process(start, state, x_position)
    begin
        -- Default assignments to prevent latches
        finish <= '0';
        next_state <= state;
        next_write_reg <= write_reg;
        next_reg <= reg;
        next_read_reg <= read_reg;
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
                    row1_buffer(x_position) <= dataR;
                elsif index_of_buffer = 1 then
                    row2_buffer(x_position) <= dataR;
                elsif index_of_buffer = 2 then
                    row3_buffer(x_position) <= dataR;
                end if;
                

                -- see if you have read the first 3 addresses of the third row and start writing
                --otherwise go to the next addresses and x_position
                if index_of_buffer = 2 and x_position = 2 then
                    -- stop reading
                    we <= '0';
                    en <= '0';
                    -- load the next computation
                    next_state <= load;
                    next_reg <= write_reg;
                    next_x_position <= x_position + 1;
                else
                    -- increment the index of buffer
                    if x_position = 87 then 
                        index_of_buffer <= index_of_buffer + 1;
                        next_x_position <= 0;
                        
                    else
                        next_x_position <= x_position + 1;
                        
                    end if;
                    next_reg <= std_logic_vector(unsigned(reg) + 1);
                end if;
                
                
                -- --start the computation when you have the two addresses of the third row
                -- if index_of_buffer = 2 and x_position = 1 then
                --     output_flag <= not output_flag;

                --     --fill computation params
                --     comp1 <= row1_buffer(1)(15 downto 0) & row1_buffer(0);
                --     comp2 <= row2_buffer(1)(15 downto 0) & row2_buffer(0);
                --     comp3 <= dataR(15 downto 0) & row3_buffer(0); --just read the second address
                -- end if;


                --start the computation when you have the two addresses of the third row
                if index_of_buffer = 2 and x_position = 1 then

                    -- start computing the first 2 pixels
                    next_comp_state <= first_computation;

                    --fill computation params. only fill:
                    -- 0, 0, pixel1, pixel2, pixel3, pixel4 (pixel of row1)
                    -- 0, 0, pixel1, pixel2, pixel3, pixel4 (pixel of row2)
                    -- 0, 0, pixel1, pixel2, pixel3, pixel4 (pixel of row3)
                    comp1(31 downto 0) <=  row1_buffer(0);
                    comp2(31 downto 0) <=  row2_buffer(0);
                    comp3(31 downto 0) <=  row3_buffer(0); 
                end if;
                
                -- take the last read registers address
                next_read_reg <= next_reg;
            when write =>
                --do something
                
                -- writing flag
                we <= '1';
                en <= '1';
       
                


                if unsigned(write_reg) = 50600 then 
                    finish <= '1';
                end if;
                
                --increment the address
                next_write_reg <= std_logic_vector(unsigned(write_reg) + 1);
                next_reg <= read_reg;

                --  shift the computation registers to the right and keep just the last byte 
                comp1 <= std_logic_vector(shift_right(unsigned(comp1), 16));
                comp2 <= std_logic_vector(shift_right(unsigned(comp2), 16));
                comp3 <= std_logic_vector(shift_right(unsigned(comp3), 16));

                --next state
                next_comp_state <= first_computation;        
                next_state <= load;
                
            when load =>
                
                --  shift the computation registers to the right and keep just the last byte 
                comp1 <= std_logic_vector(shift_right(unsigned(comp1), 16));
                comp2 <= std_logic_vector(shift_right(unsigned(comp2), 16));
                comp3 <= std_logic_vector(shift_right(unsigned(comp3), 16));
                    

                -- Checks which computation reg needs which pixels
                -- how do you increment the index ? the index represent wich position you are computing
                if index_of_buffer = 2 then                                
					comp1(47 downto 16) <= row1_buffer(index_of_computation);
					comp2(47 downto 16) <= row2_buffer(index_of_computation);
					comp3(47 downto 16) <= row3_buffer(index_of_computation);
				elsif index_of_buffer = 0 then
					comp1(47 downto 16) <= row2_buffer(index_of_computation);
					comp2(47 downto 16) <= row3_buffer(index_of_computation);
					comp3(47 downto 16) <= row1_buffer(index_of_computation);
				else 
					comp1(47 downto 16) <= row3_buffer(index_of_computation);
					comp2(47 downto 16) <= row1_buffer(index_of_computation);
					comp3(47 downto 16) <= row2_buffer(index_of_computation);
				end if;
                                -- first or second computation ?

                next_comp_state <= second_computation;
  

                -- check if you finish this row
                if index_of_computation = 87 then
                    index_of_computation <= 0;                 
                elsif index_of_computation = 0 then
                    next_comp_state <= EOL_computation;  
                    index_of_computation <= index_of_computation + 1;                    
                else
                    index_of_computation <= index_of_computation + 1;
                end if;



                           --prepare for read
                we <= '0';
                en <= '1';
                next_state <= read;
                --load the computation accordingly and start the new computation
            
            when read =>
                
                
                -- fill the correct buffer
                if index_of_buffer = 0 then 
                    row1_buffer(x_position) <= dataR;
                elsif index_of_buffer = 1 then
                    row2_buffer(x_position) <= dataR;
                elsif index_of_buffer = 2 then
                    row3_buffer(x_position) <= dataR;
                end if;


                -- increment the index of buffer
                if x_position = 87 then 
                    if index_of_buffer = 2 then
                        index_of_buffer <= 0;
                    else
                        index_of_buffer <= index_of_buffer + 1;
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
                
                
            when others =>
                next_state <= idle;
                null;
        end case;
    end process task2;
    


    compute_process : process(comp_state)
 
    variable p1, p2, p3, p4, p5, p6 : signed(8 downto 0) := (others => '0');
    variable Dx_pix1_var, Dy_pix1_var, Dx_pix2_var, Dy_pix2_var : signed(10 downto 0) := (others => '0');
    variable pix : integer := 0;
 
    begin
        next_pixel_store <= pixel_store;
        
        if EOL_flag = false then -- Check if we are the end of the buffer to stop computation     
           case(comp_state) is 
            when first_computation =>
                dataW(7 downto 0) <= pixel_store;
 
                -- Sets the pixel temp register
                p1 := to_signed(to_integer(unsigned(comp1(23 downto 16))), 9);
                p2 := to_signed(to_integer(unsigned(comp1(7 downto 0))), 9);
                p3 := to_signed(to_integer(unsigned(comp2(23 downto 16))), 9);
                p4 := to_signed(to_integer(unsigned(comp2(7 downto 0))), 9);
                p5 := to_signed(to_integer(unsigned(comp3(23 downto 16))), 9);
                p6 := to_signed(to_integer(unsigned(comp3(7 downto 0))), 9);
 
                p1_1 <= resize(p1, 11) - resize(p2, 11);
                p2_1 <= resize(2*p3, 11);
                p3_1 <= resize(2*p4, 11);
                p4_1 <= resize(p5, 11) - resize(p6, 11);
 
                Dx_pix1_var := resize(p1, 11) - resize(p2, 11) + resize(2*p3, 11) - resize(2*p4, 11) + resize(p5, 11) - resize(p6, 11);
 
                p1 := to_signed(to_integer(unsigned(comp1(7 downto 0))), 9);
                p2 := to_signed(to_integer(unsigned(comp3(7 downto 0))), 9);
                p3 := to_signed(to_integer(unsigned(comp1(15 downto 8))), 9);
                p4 := to_signed(to_integer(unsigned(comp3(15 downto 8))), 9);
                p5 := to_signed(to_integer(unsigned(comp1(23 downto 16))), 9);
                p6 := to_signed(to_integer(unsigned(comp3(23 downto 16))), 9);
 
                p1_2 <= resize(p1, 11) - resize(p2, 11);
                p2_2 <= resize(2*p3, 11);
                p3_2 <= resize(2*p4, 11);
                p4_2 <= resize(p5, 11) - resize(p6, 11);
 
                Dy_pix1_var := resize(p1, 11) - resize(p2, 11) + resize(2*p3, 11) - resize(2*p4, 11) + resize(p5, 11) - resize(p6, 11);
 
                Dx_pix1_sig <= Dx_pix1_var;
                Dy_pix1_sig <= Dy_pix1_var;
 
                pix := to_integer(abs(Dx_pix1_var) + abs(Dy_pix1_var));
                pix1_sig <= pix;
 
                -- If final value is greater than 255, clip to 255
                if pix > 255 then
                    dataW(15 downto 8) <= (others => '1');
                else
                    dataW(15 downto 8) <= std_logic_vector(to_unsigned(pix, 8));
                end if;
 
                p1 := to_signed(to_integer(unsigned(comp1(31 downto 24))), 9);
                p2 := to_signed(to_integer(unsigned(comp1(15 downto 8))), 9);
                p3 := to_signed(to_integer(unsigned(comp2(31 downto 24))), 9);
                p4 := to_signed(to_integer(unsigned(comp2(15 downto 8))), 9);
                p5 := to_signed(to_integer(unsigned(comp3(31 downto 24))), 9);
                p6 := to_signed(to_integer(unsigned(comp3(15 downto 8))), 9);
 
                Dx_pix2_var := resize(p1, 11) - resize(p2, 11) + resize(2*p3, 11) - resize(2*p4, 11) + resize(p5, 11) - resize(p6, 11);
 
                p1 := to_signed(to_integer(unsigned(comp1(15 downto 8))), 9);
                p2 := to_signed(to_integer(unsigned(comp3(15 downto 8))), 9);
                p3 := to_signed(to_integer(unsigned(comp1(23 downto 16))), 9);
                p4 := to_signed(to_integer(unsigned(comp3(23 downto 16))), 9);
                p5 := to_signed(to_integer(unsigned(comp1(31 downto 24))), 9);
                p6 := to_signed(to_integer(unsigned(comp3(31 downto 24))), 9);
 
                Dy_pix2_var := resize(p1, 11) - resize(p2, 11) + resize(2*p3, 11) - resize(2*p4, 11) + resize(p5, 11) - resize(p6, 11);
 
                Dx_pix2_sig <= Dx_pix2_var;
                Dy_pix2_sig <= Dy_pix2_var;
 
                pix := to_integer(abs(Dx_pix2_var) + abs(Dy_pix2_var));
                pix2_sig <= pix;
 
                -- If final value is greater than 255, clip to 255
                if pix > 255 then
                    dataW(23 downto 16) <= (others => '1');
                else
                    dataW(23 downto 16) <= std_logic_vector(to_unsigned(pix, 8));
                end if;

                if test_flag = true then
                    dataW(23 downto 0) <= comp2(23 downto 0);
                end if;
                -- Shifting Comp registers to place next two pixels to be computed at the same place 
            when second_computation =>       
                p1 := to_signed(to_integer(unsigned(comp1(23 downto 16))), 9);
                p2 := to_signed(to_integer(unsigned(comp1(7 downto 0))), 9);
                p3 := to_signed(to_integer(unsigned(comp2(23 downto 16))), 9);
                p4 := to_signed(to_integer(unsigned(comp2(7 downto 0))), 9);
                p5 := to_signed(to_integer(unsigned(comp3(23 downto 16))), 9);
                p6 := to_signed(to_integer(unsigned(comp3(7 downto 0))), 9);
 
                p1_1 <= resize(p1, 11) - resize(p2, 11);
                p2_1 <= resize(2*p3, 11);
                p3_1 <= resize(2*p4, 11);
                p4_1 <= resize(p5, 11) - resize(p6, 11);
 
                Dx_pix1_var := resize(p1, 11) - resize(p2, 11) + resize(2*p3, 11) - resize(2*p4, 11) + resize(p5, 11) - resize(p6, 11);
 
                p1 := to_signed(to_integer(unsigned(comp1(7 downto 0))), 9);
                p2 := to_signed(to_integer(unsigned(comp3(7 downto 0))), 9);
                p3 := to_signed(to_integer(unsigned(comp1(15 downto 8))), 9);
                p4 := to_signed(to_integer(unsigned(comp3(15 downto 8))), 9);
                p5 := to_signed(to_integer(unsigned(comp1(23 downto 16))), 9);
                p6 := to_signed(to_integer(unsigned(comp3(23 downto 16))), 9);
 
                p1_2 <= resize(p1, 11) - resize(p2, 11);
                p2_2 <= resize(2*p3, 11);
                p3_2 <= resize(2*p4, 11);
                p4_2 <= resize(p5, 11) - resize(p6, 11);
 
                Dy_pix1_var := resize(p1, 11) - resize(p2, 11) + resize(2*p3, 11) - resize(2*p4, 11) + resize(p5, 11) - resize(p6, 11);
 
                Dx_pix1_sig <= Dx_pix1_var;
                Dy_pix1_sig <= Dy_pix1_var;
 
                pix := to_integer(abs(Dx_pix1_var) + abs(Dy_pix1_var));
                pix1_sig <= pix;
 
                -- Sets the real pixel register for writing
                -- If final value is greater than 255, clip to 255
                if pix > 255 then
                    dataW(31 downto 24) <= (others => '1');
                else
                    dataW(31 downto 24) <= std_logic_vector(to_unsigned(pix, 8));
                end if;
 
                p1 := to_signed(to_integer(unsigned(comp1(31 downto 24))), 9);
                p2 := to_signed(to_integer(unsigned(comp1(15 downto 8))), 9);
                p3 := to_signed(to_integer(unsigned(comp2(31 downto 24))), 9);
                p4 := to_signed(to_integer(unsigned(comp2(15 downto 8))), 9);
                p5 := to_signed(to_integer(unsigned(comp3(31 downto 24))), 9);
                p6 := to_signed(to_integer(unsigned(comp3(15 downto 8))), 9);
 
                Dx_pix2_var := resize(p1, 11) - resize(p2, 11) + resize(2*p3, 11) - resize(2*p4, 11) + resize(p5, 11) - resize(p6, 11);
 
                p1 := to_signed(to_integer(unsigned(comp1(15 downto 8))), 9);
                p2 := to_signed(to_integer(unsigned(comp3(15 downto 8))), 9);
                p3 := to_signed(to_integer(unsigned(comp1(23 downto 16))), 9);
                p4 := to_signed(to_integer(unsigned(comp3(23 downto 16))), 9);
                p5 := to_signed(to_integer(unsigned(comp1(31 downto 24))), 9);
                p6 := to_signed(to_integer(unsigned(comp3(31 downto 24))), 9);
 
                Dy_pix2_var := resize(p1, 11) - resize(p2, 11) + resize(2*p3, 11) - resize(2*p4, 11) + resize(p5, 11) - resize(p6, 11);
 
                Dx_pix2_sig <= Dx_pix2_var;
                Dy_pix2_sig <= Dy_pix2_var;
 
                pix := to_integer(abs(Dx_pix2_var) + abs(Dy_pix2_var));
                pix2_sig <= pix;
 
                -- Store last pixel for next address
                -- If final value is greater than 255, clip to 255
                if pix > 255 then
                    next_pixel_store <= (others => '1');
                else
                    next_pixel_store <= std_logic_vector(to_signed(pix, 8));
                end if;

                if test_flag = true then
                    dataW(31 downto 24) <= comp2(15 downto 8);  -- Output the read pixels
                end if;
                
            when EOL_computation =>
                dataW(31 downto 24) <= (others => '0');
                next_pixel_store <= (others => '0');
            when others =>
               null;
            end case;
        --  else
        --      dataW(31 downto 24) <= (others => '0');
        --      next_pixel_store <= (others => '0');
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
                pixel_out <= next_pixel_out;
                state <= next_state;
                reg <= next_reg;
                write_reg <= next_write_reg;
                read_reg <= next_read_reg;
                -- index_of_load <= next_index_of_load;
                addr <= next_reg;
                y_position <= next_y_position;
                x_position <= next_x_position;
                pixel_store <= next_pixel_store;
                comp_state <= next_comp_state;
				--prev_x_position <= x_position;
            end if;
        end if;
    end process register_process;

end rtl;