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
	begin
		return false;
	end is_valid;

	procedure New_Validation_Period( Entity : in Entity_Type; From_Date, To_Date : in Validation_Timestamp ) is
		-- set the new validation period.
		-- raise INVALID_PERIOD when From_Date > To_Date or Is_Valid == true
	begin
		null;
	end New_Validation_Period;



	procedure Expire( Entity : in Entity_Type; To_Date : in Validation_Timestamp ) is
		-- expire in the given date..
		-- raise invalid_period when To_Date < the last validation period
	begin
		null;
	end Expire;


	procedure Validate( Entity : in Entity_Type; From_Date : in Validation_Timestamp ) is
		-- validate from the given date..
		--
		-- raise invalid_period if Is_Valid = TRUE
	begin
		null;
	end Validate;



	procedure Expire_Now( Entity : in Entity_Type ) is
		-- expire now
		-- raise INVALID_PERIOD if Is_Valid = FALSE
	begin
		null;
	end Expire_Now;


	procedure Validate_Now( Entity : in Entity_Type ) is
		-- raise invalid_period if Is_Valid = TRUE
	begin
		null;
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
