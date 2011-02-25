------------------------------------------------------------------------------
--                                                                          --
--                        KOW Framework :: Entity                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2007-2011, KOW Framework Project               --
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
with Ada.Calendar;		use Ada.Calendar;
with Ada.Containers;


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Ent.Properties;

-- Some information:
-- 	there can be only 1 validation period active act the time.
-- 	it means the 


package body KOW_Ent.Expirable_Entity_Controls is



	-- --------------------- --
	-- Validation Management --
	-- --------------------- --
	

	overriding
	procedure Will_Insert( Validation : in out Validation_Entity ) is
		-- make sure the validation period is actually valid..
	begin

		if Validation.From_Date = No_Validation OR Validation.To_Date = No_Validation then
			return;
		elsif Validation.From_Date > Validation.To_Date then
			raise CONSTRAINT_ERROR with "invalid period!";
		end if;
	end Will_Insert;

	overriding
	procedure Will_Update( Validation : in out Validation_Entity ) is
		-- make sure the validation period is actually valid..
	begin
		Will_Insert( Validation );
	end Will_Update;



	function Is_Valid( Entity : in Entity_Type ) return Boolean is
		-- check if the entity is in a valid period
		Validation : Validation_Entity;
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
		Validation : Validation_Entity;
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
		Validation : Validation_Entity := Last_Validation( Entity );
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
		Validation : Validation_Entity;
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

	
	function Last_Validation( Entity : in Entity_Type ) return Validation_Entity is
		-- get the last validation in the database backend
		Validations : Validation_Array := Get_Validations( Entity );
		-- TODO :: implement Last_Validation using Query_Builders
	begin
		return Validations( Validations'Last );
	end Last_Validation;



	function Get_Validations( Entity : in Entity_Type ) return Validation_Array is
		-- get all the registered validation entities
		Query	: Query_Builders.Query_Type;
		Results	: Query_Builders.Entity_Vectors.Vector;

		use Query_Builders;
	begin
		Append( Q => Query, Foreign_key => Entity );
		Append_Order( Q => Query, Column => "from_date" );

		Results := Get_All( Query );


		declare
			Count : Integer := Integer( Query_Builders.Entity_Vectors.Length( Results ) );
			Ret   : Validation_Array( 1 .. Count );
		begin
			for i in 1 .. count loop
				Ret( i ) := Query_Builders.Entity_Vectors.Element( Results, i );
			end loop;

			return Ret;
		end;
	end Get_Validations;


























	--
	-- now the private entity handling part of this package :)
	--


	--
	-- the factory
	--
	function Validation_Factory return KOW_Ent.Entity_Type'Class is
		E : Validation_Entity;
	begin
		return E;
	end Validation_Factory;

	--
	-- from date
	--
	function get_From_Date( E : in KOW_Ent.Entity_Type'Class ) return Validation_Timestamp is
	begin
		return Validation_Entity( E ).From_Date;
	end get_From_Date;

	procedure set_From_Date( E : in out KOW_Ent.Entity_Type'Class; From_Date : in Validation_Timestamp ) is
	begin
		Validation_Entity( E ).From_Date := From_Date;
	end set_From_Date;

	--
	-- to date
	--
	function get_To_Date( E : in KOW_Ent.Entity_Type'Class ) return Validation_Timestamp is
	begin
		return Validation_Entity( E ).To_Date;
	end get_To_Date;

	procedure set_To_Date( E : in out KOW_Ent.Entity_Type'Class; To_Date : in Validation_Timestamp ) is
	begin
		Validation_Entity( E ).To_Date := To_Date;
	end set_To_Date;

	--
	-- owner id
	--
	function get_Owner_Id( E : in KOW_Ent.Entity_Type'Class ) return KOW_Ent.ID_Type is
	begin
		return Validation_Entity( E ).Owner_Id;
	end get_Owner_Id;

	procedure set_Owner_Id( E : in out KOW_Ent.Entity_Type'Class; Owner_ID : in KOW_Ent.Id_Type ) is
	begin
		Validation_Entity( E ).Owner_ID := Owner_ID;
	end set_Owner_Id;
	


begin





	KOW_Ent.Entity_Registry.Register(
			Entity_Tag	=> Validation_Entity'Tag,
			Table_Name	=> Table_Name,
			Id_Generator	=> null,
			Factory		=> Validation_Factory'Unrestricted_Access
		);


	KOW_Ent.Entity_Registry.Add_Property(
			Entity_Tag	=> Validation_Entity'Tag,
			Property	=> KOW_Ent.Extra_Properties.Timestamp_Properties.New_Property(
							Column_Name		=> "from_date",
							Getter			=> Get_From_Date'Unrestricted_Access,
							Setter			=> Set_From_Date'Unrestricted_Access
						)
		);

	KOW_Ent.Entity_Registry.Add_Property(
			Entity_Tag	=> Validation_Entity'Tag,
			Property	=> KOW_Ent.Extra_Properties.Timestamp_Properties.New_Property(
							Column_Name		=> "to_date",
							Getter			=> Get_To_Date'Unrestricted_Access,
							Setter			=> Set_To_Date'Unrestricted_Access
						)
		);

	

	KOW_Ent.Entity_Registry.Add_Property(
			Entity_Tag	=> Validation_Entity'Tag,
			Property	=> KOW_Ent.Properties.New_Foreign_Key_Property(
							Column_Name		=> "owner_id",
							Related_Entity_Tag	=> Entity_Type'Tag,
							Getter			=> Get_Owner_Id'Unrestricted_Access,
							Setter			=> Set_Owner_Id'Unrestricted_Access
						)
		);







end KOW_Ent.Expirable_Entity_Controls;
