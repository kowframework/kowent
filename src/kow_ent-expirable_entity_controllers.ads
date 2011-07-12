------------------------------------------------------------------------------
--                                                                          --
--                       KOW Framework :: Entities                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOWLib is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOWLib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with KOWLib; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

-----------------------------------------------------------------------------
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
with KOW_Ent.Extra_Properties;
with KOW_Ent.Generic_Query_Builders;





-- Some information:
-- 	there can be only 1 validation period active act the time.
-- 	it means the 


generic
	type Entity_Type is new KOW_Ent.Entity_Type with private;
	Table_Name : String;
package KOW_Ent.Expirable_Entity_Controllers is



	package Entity_Vectors is new Ada.Containers.Vectors(
				Element_Type	=> Entity_Type,
				Index_Type	=> Positive
			);

	INVALID_PERIOD : Exception;
	-- raised when there is an incosistence in the validation period

	NO_VALIDATION  : Exception;
	-- raised when there is no validation entity in such time

	--------------------------
	-- Validation Timestamp --
	--------------------------

	subtype Validation_Timestamp is KOW_Ent.Extra_Properties.Timestamp;

	function Clock return Validation_Timestamp;
	-- same as Ada.Calendar.Clock


	No_Validation_Timestamp : constant Validation_Timestamp := KOW_Ent.Extra_Properties.No_Timestamp;



	-----------------------
	-- Validation Entity --
	-----------------------

	type Validation_Entity is new KOW_Ent.Entity_Type with record
		From_Date	: Validation_Timestamp := No_Validation_Timestamp;
		To_Date		: Validation_Timestamp := No_Validation_Timestamp;
		Owner_ID	: KOW_Ent.ID_Type;
	end record;

	overriding
	procedure Will_Insert(
				Validation	: in out Validation_Entity
			);
	-- make sure the validation period is actually valid and doesnt intersect with other validation period
	
	overriding
	procedure Will_Update(
				Validation	: in out Validation_Entity
			);
	-- make sure the validation period is actually valid and doesnt intersect with other validation period
	

	function Get_Validation(
				Entity		: in     Entity_Type;
				For_Date	: in     Validation_Timestamp := Clock
			) return Validation_Entity'Class;
	-- get the validation entity for the given entity during For_Date
	-- raise NO_VALIDATION if none found


	package Query_Builders is new KOW_Ent.Generic_Query_Builders( Entity_Type => Validation_Entity );


	-----------
	-- Array --
	-----------

	type Validation_Array is Array( Positive range <> ) of Validation_Entity;

	function Get_Validations(
				Entity	: in     Entity_Type
			) return Validation_Array;
	-- get all the registered validation entities




	
	-----------
	-- Query --
	-----------

	

	function Is_Valid(
				Entity		: in Entity_Type;
				From_Date	: in Validation_Timestamp;
				To_Date		: in Validation_Timestamp
			) return Boolean;
	-- check if the entity is valid anytime durin the given period

	function Is_Valid(
				Entity		: in Entity_Type;
				Date		: in Validation_Timestamp := Clock
			) return Boolean;
	-- check if the entity is valid in the given instant


	--function Query_Valid( Date : in Validation_Timestamp := Clock ) return KOW_Ent.Id_Array_Type;
	-- quere the active entities

	------------
	-- Create --
	------------

	procedure New_Validation_Period(
				Entity			: in     Entity_Type;
				From_Date, To_Date	: in     Validation_Timestamp
			);
	-- set the new validation period.
	-- raise INVALID_PERIOD when From_Date > To_Date or Is_Valid == true


	------------
	-- Manage --
	------------

	procedure Expire(
				Entity	: in     Entity_Type;
				To_Date	: in     Validation_Timestamp := Clock
			);
	-- expire in the given date..
	-- raise invalid_period when To_Date < the last validation period
	-- raise no_validation when it's not valid during the specified time
	
	procedure Validate(
				Entity		: in     Entity_Type; 
				From_Date	: in     Validation_Timestamp := Clock
			);
	-- validate from the given date..
	--
	-- if there is a validation period just before From_Date, copy
	-- that into the current one;
	--
	-- this will easy the work in case you extend the validation_entity type
	--
	-- raise invalid_period if Is_Valid = TRUE


end KOW_Ent.Expirable_Entity_Controllers;
