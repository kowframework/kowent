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



--------------
-- Ada 2005 --
--------------
with Ada.Calendar;
with Ada.Containers.Vectors;


---------
-- APQ --
---------
with APQ;


-------------------
-- KOW Framework --
-------------------
with KOW_Ent.Query_Builders;





-- Some information:
-- 	there can be only 1 validation period active act the time.
-- 	it means the 


generic
	type Entity_Type is new KOW_Ent.Entity_Type with private;
	Table_Name : String;
package KOW_Ent.Expirable_Entity_Controls is



	package Entity_Vectors is new Ada.Containers.Vectors(
				Element_Type	=> Entity_Type,
				Index_Type	=> Positive
			);

	INVALID_PERIOD : Exception;
	-- raised when there is an incosistence in the validation period

	-- -------------------- --
	-- Validation Timestamp --
	-- -------------------- --

	type Validation_Timestamp is new Ada.Calendar.Time;


        function Timestamp_To_String is new APQ.Timestamp_String( Val_Type => Validation_Timestamp );

	function String_to_Timestamp is new APQ.Convert_To_Timestamp( Val_Type => Validation_Timestamp );


	No_Validation : constant Validation_Timestamp := Validation_Timestamp( Ada.Calendar.Clock );
	-- todo :: change it to a decent implementation



	


	

	-- --------------------- --
	-- Validation Management --
	-- --------------------- --
	


	function Is_Valid( Entity : in Entity_Type ) return Boolean;
	-- check if the entity is in a valid period

	procedure New_Validation_Period( Entity : in Entity_Type; From_Date, To_Date : in Validation_Timestamp );
	-- set the new validation period.
	-- raise INVALID_PERIOD when From_Date > To_Date or Is_Valid == true

	procedure Expire( Entity : in Entity_Type; To_Date : in Validation_Timestamp );
	-- expire in the given date..
	-- raise invalid_period when To_Date < the last validation period
	
	procedure Validate( Entity : in Entity_Type; From_Date : in Validation_Timestamp );
	-- validate from the given date..
	--
	-- raise invalid_period if Is_Valid = TRUE

	procedure Expire_Now( Entity : in Entity_Type );
	-- expire now
	-- raise INVALID_PERIOD if Is_Valid = FALSE
	

	procedure Validate_Now( Entity : in Entity_Type );
	-- raise invalid_period if Is_Valid = TRUE

	type Validation_Type is new KOW_Ent.Entity_Type with record
		From_Date	: Validation_Timestamp;
		To_Date		: Validation_Timestamp;
		Owner_ID	: KOW_Ent.ID_Type;
	end record;
	
	function Last_Validation( Entity : in Entity_Type ) return Validation_Type;
	-- get the last validation in the database backend


	type Validation_Array is Array( Positive range <> ) of Validation_Type;

	function Get_Validations( Entity : in Entity_Type ) return Validation_Array;
	-- get all the registered validation entities



	package Query_Builders is new KOW_Ent.Query_Builders( Entity_Type => Validation_Type );


end KOW_Ent.Expirable_Entity_Controls;
