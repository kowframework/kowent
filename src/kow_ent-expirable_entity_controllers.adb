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
with KOW_Ent.Id_Query_Builders;
with KOW_Ent.Properties;

-- Some information:
-- 	there can be only 1 validation period active act the time.
-- 	it means the 


package body KOW_Ent.Expirable_Entity_Controllers is





	--------------------
	-- Helper Methods --
	--------------------


	procedure Check_New( Entity : in KOW_Ent.Entity_Type'Class ) is
	begin
		if KOW_Ent.Is_New( Entity ) then
			raise CONSTRAINT_ERROR with "can't do this in a new entity";
		end if;
	end Check_New;

	procedure Append_In_Period(
				Q	: in out Query_Builders.Entity_Query_Type;
				Value	: in     Validation_Timestamp;
				Appender: in     KOW_Ent.Id_Query_Builders.Logic_Appender
			) is
		use Query_Builders;
		use KOW_Ent.Id_Query_Builders;
		Child_Q : Entity_Query_Type;


	begin
		Prepare( Child_Q, Validation_Entity'Tag );

		Append_Timestamp(
				Q		=> Child_Q,
				Column		=> "from_date",
				Value		=> Value,
				Operator	=> Operator_Less_Than_Or_Equal_To
			);

		declare
			Sub_Child_Q : Entity_Query_Type;
		begin
			Append_Timestamp(
					Q		=> Sub_Child_Q,
					Column		=> "to_date",
					Value		=> Value,
					Operator	=> Operator_Greater_Than_Or_Equal_To
				);
			-- has expiration date...
			Append_Timestamp(
					Q		=> Sub_Child_Q,
					Column		=> "to_date",
					Value		=> No_Validation_Timestamp,
					Operator	=> Operator_Less_Than_Or_Equal_To,
					Appender	=> Appender_OR
				);

			Append(
					Q	=> Child_Q,
					Child_Q	=> Sub_Child_Q,
					Appender=> Appender_AND
				);
		end;

		Append(
				Q		=> Q,
				Child_Q		=> Child_Q,
				Appender	=> Appender
			);
	end Append_In_Period;



	function Is_Valid_Query(
				From_Date	: in Validation_Timestamp;
				To_Date		: in Validation_Timestamp
			) return Query_Builders.Entity_Query_Type is
		-- return a query type initialized for when it's valid..
		use Query_Builders;

		Child_Q : Entity_Query_Type;
		procedure Append_Inside( Date : in Validation_Timestamp ) is
			use KOW_Ent.Id_Query_Builders;
		begin
			Append_In_Period(
					Q		=> Child_Q,
					Value		=> Date,
					Appender	=> Appender_OR
				);
		end Append_Inside;


		procedure Append_Outside is
			use KOW_Ent.Id_Query_Builders;
			Sub_Child_Q : Entity_Query_Type;
		begin
			Prepare( Sub_Child_Q, Validation_Entity'Tag );
			Append_Timestamp(
					Q		=> Sub_Child_Q,
					Column		=> "from_date",
					Value		=> From_Date,
					Operator	=> Operator_Greater_Than
				);
			Append_Timestamp(
					Q		=> Sub_Child_Q,
					Column		=> "to_date",
					Value		=> To_Date,
					Operator	=> Operator_Less_Than
				);

			Append(
					Q		=> Child_Q,
					Child_Q		=> Sub_Child_Q,
					Appender	=> Appender_OR
				);
		end Append_Outside;
	begin
		Prepare( Child_Q, Validation_Entity'Tag );
		Append_Inside( From_Date );
		Append_Inside( To_Date );
		Append_Outside;

		return Child_Q;
	end Is_Valid_Query;




	--------------------------
	-- Validation Timestamp --
	--------------------------

	function Clock return Validation_Timestamp is
		-- same as Ada.Calendar.Clock
	begin
		return Validation_Timestamp( Ada.Calendar.Clock );
	end Clock;

	-----------------------
	-- Validation Entity --
	-----------------------
	

	overriding
	procedure Will_Insert(
				Validation	: in out Validation_Entity
			) is
		-- make sure the validation period is actually valid..
	begin

		if Validation.From_Date = No_Validation_Timestamp OR Validation.To_Date = No_Validation_Timestamp then
			return;
		elsif Validation.From_Date > Validation.To_Date then
			raise INVALID_PERIOD;
		end if;
	end Will_Insert;



	overriding
	procedure Will_Update(
				Validation	: in out Validation_Entity
			) is
		-- make sure the validation period is actually valid..
	begin
		Will_Insert( Validation );
	end Will_Update;


	function Get_Validation(
				Entity	: in     Entity_Type;
				For_Date: in     Validation_Timestamp := Clock
			) return Validation_Entity'Class is
		use Query_Builders;


		Q : Entity_Query_Type;
	begin
		Append(
				Q	=> Q,
				Column	=> "owner_id",
				Value	=> Entity.ID
			);
		Append_In_Period(
				Q	=> Q,
				Value	=> For_Date,
				Appender=> KOW_Ent.Id_Query_Builders.Appender_And
			);

		if Count( Q ) /= 1 then
			raise NO_VALIDATION with "it's not actually valid in the given period";
		end if;

		return Validation_Entity'Class( KOW_Ent.Narrow( Get_First( Q, True ) ) );
	end Get_Validation;



	-----------
	-- Array --
	-----------
	function Get_Validations(
				Entity	: in     Entity_Type
			) return Validation_Array is
		-- get all the registered validation entities


		Query	: Query_Builders.Entity_Query_Type;
		Results	: Query_Builders.Entity_Vectors.Vector;

		use Query_Builders;
	begin
		Check_New( Entity );

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




	-----------
	-- Query --
	-----------

	function Is_Valid(
				Entity		: in Entity_Type;
				From_Date	: in Validation_Timestamp;
				To_Date		: in Validation_Timestamp
			) return Boolean is
		-- check if the entity is valid anytime durin the given period
		use Query_Builders;
		Q : Entity_Query_Type;


	begin
		Check_New( Entity );

		Append(
				Q	=> Q,
				Column	=> "owner_id",
				Value	=> Entity.Id
			);

		Append(
				Q	=> Q,
				Child_Q	=> Is_Valid_Query( From_Date => From_Date, To_Date => To_Date ),
				Appender=> KOW_Ent.Id_Query_Builders.Appender_And
			);

		return Count( Q ) = 1;
	end Is_Valid;


	function Is_Valid(
				Entity		: in Entity_Type;
				Date		: in Validation_Timestamp := Clock
			) return Boolean is
		-- check if the entity is valid in the given instant
	begin
		return Is_Valid( Entity, Date, Date );
	end Is_Valid;



	function Query_Valid(
				Date		: in     Validation_Timestamp := Clock;
				From		: in     Positive := 1;
				Limit		: in     Natural := 0
			) return KOW_Ent.Id_Array_Type is
		-- quere the active entities
		use KOW_Ent.Id_Query_Builders;

		Q	: Query_Type;
	begin
		Prepare( Q, Entity_Type'Tag );
		Append(
				Q	=> Q,
				Child_Q	=> Query_Type( Is_Valid_Query( From_Date => Date, To_Date => Date ) ),
				Appender=> Appender_And
			);

		return Get_Some(
				Q	=> Q,
				From	=> From,
				Limit	=> Limit
			);
	end Query_Valid;


	------------
	-- Create --
	------------


	procedure New_Validation_Period(
				Entity			: in     Entity_Type;
				From_Date, To_Date	: in     Validation_Timestamp
			) is
		-- set the new validation period.
		-- raise INVALID_PERIOD when From_Date > To_Date or Is_Valid == true

		Validation : Validation_Entity;
	begin
		Check_New( Entity );

		if Is_Valid( Entity, From_Date, To_Date ) then
			raise INVALID_PERIOD with "intersects with other validation period";
		end if;

		Validation.From_Date := From_Date;
		Validation.To_Date := To_Date;
		Validation.Owner_Id := Entity.ID;

		Store( Validation );
	end New_Validation_Period;



	------------
	-- Manage --
	------------


	procedure Expire(
				Entity	: in     Entity_Type;
				To_Date	: in     Validation_Timestamp := Clock
			) is
		-- expire in the given date..
		-- raise invalid_period when To_Date < the last validation period

		Validation : Validation_Entity'Class := Get_Validation( Entity, To_Date );
	begin
		Check_New( Entity );

		if Validation.To_Date > No_Validation_Timestamp AND THEN Validation.To_Date < To_Date then
			raise INVALID_PERIOD with "Invalidating an already invalid entity";
		end if;
		Validation.To_Date := To_Date;
		Store( Validation );
	end Expire;




	procedure Validate(
				Entity		: in     Entity_Type;
				From_Date	: in     Validation_Timestamp := Clock
			) is
		-- validate from the given date..
		--
		-- raise invalid_period if Is_Valid = TRUE


		use Query_Builders;



		function Get_Validation return Validation_Entity'Class is
			use KOW_Ent.Id_Query_Builders;
			Previous_Q : Entity_Query_Type;
		begin
			Append(
					Q	=> Previous_Q,
					Column	=> "owner_id",
					Value	=> Entity.ID
				);


			Append_Timestamp(
					Q	=> Previous_Q,
					Column	=> "to_date",
					Value	=> From_Date,
					Operator=> Operator_Less_Than
				);


			Append_Order(
					Q		=> Previous_Q,
					Column		=> "to_date",
					Ordenation	=> DESCENDING
				);



			if Count( Previous_Q ) >= 1 then
				declare
					No_ID : KOW_Ent.Id_Type;
					Validation : Validation_Entity'Class := Validation_Entity'Class( KOW_Ent.Narrow( Get_First( Previous_Q, False ) ) );
				begin
					Validation.ID := No_ID;
					return Validation;
				end;
			else
				declare
					New_Validation : Validation_Entity;
				begin
					return New_Validation;
				end;
			end if;
		end Get_Validation;


		Validation	: Validation_Entity'Class := Get_Validation;
		Q		: Entity_Query_Type;
	begin
		Check_New( Entity );

		-- Check if it's valid ...
		if Is_Valid( Entity, From_Date ) then
			raise INVALID_PERIOD with "Trying to validate an already valid entity";
		end if;


		Validation.From_Date := From_Date;
		Validation.To_Date   := No_Validation_Timestamp;
		Validation.Owner_Id  := Entity.ID;

		Store( Validation );
	end Validate;




























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







end KOW_Ent.Expirable_Entity_Controllers;
