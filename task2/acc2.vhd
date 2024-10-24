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

type state_type is ( S0, S1, S2, S3);

signal state, next_state : state_type;
signal i : integer;

begin

    next_state <= state;
    

    cl : process(start)
    begin
    
        case (state) is
            when S0 =>
                if start='1' then
                    addr <= std_logic_vector(to_unsigned(i, addr'length));
                    next_state <= S1;
                end if;
            when S1 =>
                en <= '1';
                next_state<= S2;
            when S2 =>
                we <= '1';
                dataW <= not(dataR);
                addr <= std_logic_vector(to_unsigned(i + 25344, addr'length)) ;
                next_state<= S3;
            when S3 =>
                if i /= 25344 then
                    we <= '0';
                    i <= i  +1;
                else
                    we <= '0';
                    en <= '0';
                    finish <= '1';
                end if;
                next_state<= S0;
        end case;    
        
        
    end process cl;

-- Template for a process
    register_process : process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                addr <= (others => '0');
                i <= 0;
            else
                -- Registers update
                state <= next_state;
            end if;
        end if;
    end process register_process;

end rtl;
