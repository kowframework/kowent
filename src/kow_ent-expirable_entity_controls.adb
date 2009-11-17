------------------------------------------------------------------------------
--                                                                          --
--                        KOW Framework :: Entity                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2007-2009, KOW Framework Project               --
--                                                                          --
--                                                                          --
-- KOW_Ent is free library;  you can redistribute it  and/or modify it under--
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOW_Ent is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with KOW_Ent; see file COPYING. If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- This package provides a generic implementation of an entity to control   --
-- validation periods for entities.                                         --
------------------------------------------------------------------------------



-- Some information:
-- 	there can be only 1 validation period active act the time.
-- 	it means the 


package body KOW_Ent.Expirable_Entity_Controls is


	------------------------------------
	-- General Purpose Query Entities --
	------------------------------------
	

	

	function Is_Valid( Entity : in Entity_Type ) return Boolean is
		-- check if the entity is in a valid period
		Validation : Validation_Type;
		Now : Validation_Timestamp := Validation_Timestamp( Ada.Calendar.Clock );
	begin
		Validation := Last_Validation( Entity );
		return Validation.From_Date < Now AND ( Validation.To_Date > Now OR Validation.To_Date = No_Validation );
	exception
		when INVALID_PERIOD => 
			return false;
	end is_valid;

	procedure New_Validation_Period( Entity : in Entity_Type; From_Date, To_Date : in Validation_Timestamp ) is
		-- set the new validation period.
		-- raise INVALID_PERIOD when From_Date > To_Date or Is_Valid == true
		Validation : Validation_Type;
	begin
		if To_Date /= No_Validation AND THEN From_Date > To_Date then
			raise INVALID_PERIOD with "From_Date can't be bigger than To_Date";
		elsif Is_Valid( Entity ) then
			raise INVALID_PERIOD with "Can't set period when the entity is already valid";
		end if;

		Validation.From_Date := From_Date;
		Validation.To_Date := To_Date;
		Validation.Owner_Id := Entity.ID;

		Store( Validation );

	end New_Validation_Period;



	procedure Expire( Entity : in Entity_Type; To_Date : in Validation_Timestamp ) is
		-- expire in the given date..
		-- raise invalid_period when To_Date < the last validation period
		Validation : Validation_Type := Last_Validation( Entity );
	begin
		if Validation.To_Date /= No_Validation AND THEN Validation.To_Date < To_Date then
			raise INVALID_PERIOD with "Invalidating an already invalid entity";
		end if;
		Validation.To_Date := To_Date;
		Store( Validation );
	end Expire;


	procedure Validate( Entity : in Entity_Type; From_Date : in Validation_Timestamp ) is
		-- validate from the given date..
		--
		-- raise invalid_period if Is_Valid = TRUE
		Validation : Validation_Type;
	begin
		if Is_Valid( Entity ) then
			raise INVALID_PERIOD with "Trying to validate an already valid entity";
		end if;

		Validation.From_Date := From_Date;
		Validation.To_Date := No_Validation;
		Validation.Owner_Id := Entity.ID;

		Store( Validation );

	end Validate;



	procedure Expire_Now( Entity : in Entity_Type ) is
		-- expire now
		-- raise INVALID_PERIOD if Is_Valid = FALSE
	begin
		Expire( Entity, Validation_Timestamp( Ada.Calendar.Clock ) );
	end Expire_Now;


	procedure Validate_Now( Entity : in Entity_Type ) is
		-- raise invalid_period if Is_Valid = TRUE
	begin
		Validate( Entity, Validation_Timestamp( Ada.Calendar.Clock ) );
	end Validate_Now;

	
	function Last_Validation( Entity : in Entity_Type ) return Validation_Type is
		-- get the last validation in the database backend
		Ret : Validation_Type;
	begin
		return Ret;
	end Last_Validation;



	function Get_Validations( Entity : in Entity_Type ) return Validation_Array is
		-- get all the registered validation entities
		Ret : Validation_Array( 1 .. 1 );
	begin
		return Ret;
	end Get_Validations;

end KOW_Ent.Expirable_Entity_Controls;
