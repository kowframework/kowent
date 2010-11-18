------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Entity                             --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2007-2009, Ada Works Project                 --
--                                                                          --
--                                                                          --
-- KOW_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOW_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with KOW_Lib; see file COPYING. If not, write --
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



-- This package contains all from factory supported property type handlers.
--
-- TODO: think about changin all the above implementation into generic packages, one for each
-- property type and then instanciate all of them here for the lazy ones

---------------
-- Ada Works --
---------------
with APQ;
with KOW_Ent;
with KOW_Lib.Locales;


package KOW_Ent.Properties is

	-----------------
	-- Foreign Key --
	-----------------
	
	type Foreign_Key_Getter_Callback is not null access function( Entity : in KOW_Ent.Entity_Type'Class ) return KOW_Ent.ID_Type;
	type Foreign_Key_Setter_Callback is not null access procedure( Entity : in out KOW_Ent.Entity_Type'Class; ID : in KOW_Ent.ID_Type );


	type Foreign_Key_Property_Type is new Entity_Property_Type with record
		Related_Entity_Tag	: Ada.Tags.Tag;
		Getter			: Foreign_Key_Getter_Callback;
		Setter			: Foreign_Key_Setter_Callback;
	end record;


	overriding
	procedure Set_Property(	
				Property	: in     Foreign_Key_Property_Type;	-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			);
	-- Set the property into the Entity.

	overriding
	procedure Get_Property(
				Property	: in     Foreign_Key_Property_Type;	-- the property worker
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			);

	overriding
	procedure Set_Property(
				Property	: in     Foreign_Key_Property_Type;	-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			);
	-- Set the property from a String representation of the value
	
	overriding
	function Get_Property(
				Property	: in     Foreign_Key_Property_Type;	-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String;

	overriding
	procedure Append_Create_Table( Property : in Foreign_Key_Property_Type; Query : in out APQ.Root_Query_Type'Class );




	function New_Foreign_Key_Property(
				Column_Name		: in String;
				Related_Entity_Tag	: in Ada.Tags.Tag;
				Getter			: Foreign_Key_Getter_Callback;
				Setter			: Foreign_Key_Setter_Callback;
				Immutable		: Boolean := False
			) return Entity_Property_Ptr;


	----------------------
	-- Boolean Property --
	----------------------

	type Boolean_Getter_Callback is not null access function( Entity : in KOW_Ent.Entity_Type'Class ) return Boolean;
	type Boolean_Setter_Callback is not null access procedure( Entity : in out KOW_ent.Entity_Type'Class; Value : in Boolean );


	type Boolean_Property_Type is new Entity_Property_Type with record
		Getter			: Boolean_Getter_Callback;
		Setter			: Boolean_Setter_Callback;
	end record;


	overriding
	procedure Set_Property(	
				Property	: in     Boolean_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			);
	-- Set the property into the Entity.

	overriding
	procedure Get_Property(
				Property	: in     Boolean_Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			);

	overriding
	procedure Set_Property(
				Property	: in     Boolean_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			);
	-- Set the property from a String representation of the value
	
	overriding
	function Get_Property(
				Property	: in     Boolean_Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String;

	overriding
	procedure Append_Create_Table( Property : in Boolean_Property_Type; Query : in out APQ.Root_Query_Type'Class );


	function New_Boolean_Property(
				Column_Name		: in String;
				Getter			: Boolean_Getter_Callback;
				Setter			: Boolean_Setter_Callback;
				Immutable		: Boolean := False
			) return Entity_Property_Ptr;



	---------------------
	-- Locale Property --
	---------------------

	type Locale_Getter_Callback is not null access function( Entity : in Entity_Type'Class ) return KOW_Lib.Locales.Locale;
	type Locale_Setter_Callback is not null access procedure( Entity : in out Entity_Type'Class; Locale : in KOW_Lib.Locales.Locale );


	type Locale_Property_Type is new Entity_Property_Type with record
		Getter	: Locale_Getter_Callback;
		Setter	: Locale_Setter_Callback;
	end record;


	overriding
	procedure Set_Property(	
				Property	: in     Locale_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			);
	-- Set the property into the Entity.

	overriding
	procedure Get_Property(
				Property	: in     Locale_Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			);

	overriding
	procedure Set_Property(
				Property	: in     Locale_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			);
	-- Set the property from a String representation of the value
	
	overriding
	function Get_Property(
				Property	: in     Locale_Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String;


	overriding
	procedure Append_Create_Table( Property : in Locale_Property_Type; Query : in out APQ.Root_Query_Type'Class );



	function New_Locale_Property(
				Column_Name		: in String;
				Getter			: Locale_Getter_Callback;
				Setter			: Locale_Setter_Callback;
				Immutable		: Boolean := False
			) return Entity_Property_Ptr;




	-------------------------------
	-- Unbounded String property --
	-------------------------------

	type UString_Getter_Callback is not null access function( Entity : in KOW_Ent.Entity_Type'Class ) return Unbounded_String;
	type UString_Setter_Callback is not null access procedure( Entity : in out KOW_Ent.Entity_Type'Class; Value : in Unbounded_String );

	type UString_Property_Type is new Entity_Property_Type with record
		Getter		: UString_Getter_Callback;
		Setter		: UString_Setter_Callback;
		Default_Value	: Unbounded_String;
		Length		: Positive;
	end record;


	overriding
	procedure Set_Property(	
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			);
	-- Set the property into the Entity.

	overriding
	procedure Get_Property(
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			);



	overriding
	procedure Set_Property(
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			);
	-- Set the property from a String representation of the value
	
	overriding
	function Get_Property(
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String;


	overriding
	procedure Append_Create_Table( Property : in UString_Property_Type; Query : in out APQ.Root_Query_Type'Class );


	function New_UString_Property(
				Column_Name	: in     String;
				Getter		: UString_Getter_Callback;
				Setter		: UString_Setter_Callback;
				Default_Value	: in     String := "N/A";
				Immutable	: in     Boolean := False;
				Length		: in     Positive := 150

			) return Entity_Property_Ptr;
	-- used to assist the creation of UString properties.
	-- default_value represents the value to be set when the one retoner from database is NULL


	-----------------------
	-- Password Property --
	-----------------------

	type Password_Getter_Callback is not null access function( Entity : in KOW_Ent.Entity_Type'Class ) return Unbounded_String;

	type Password_Setter_Callback is not null access procedure( Entity : in out KOW_Ent.Entity_Type'Class; Password : in Unbounded_String );
	-- NOTE:: the password setter type CAN be null!
		

	type Password_Property_Type is new Entity_Property_Type with record
		-- A password property is a property that never reads from the database
		-- it's only used to set a new password.
		--
		-- The stored password is Hashed.
		Getter : Password_Getter_Callback;
		-- get the non-hashed password


		Setter : Password_Setter_Callback;
	end record;



	overriding
	procedure Set_Property(	
				Property	: in     Password_Property_Type;	-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			) is null;
	-- Set the property into the Entity.
	-- This procedure does nothin'

	overriding
	procedure Get_Property(
				Property	: in     Password_Property_Type;	-- the property worker
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			);

	overriding
	procedure Set_Property(
				Property	: in     Password_Property_Type;	-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			);
	-- Set the property from a String representation of the value
	
	overriding
	function Get_Property(
				Property	: in     Password_Property_Type;	-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String;
		
	
	overriding
	function Should_Read( Property : in Password_Property_Type ) return Boolean;
	-- The password should never be read from the database, so this is == false

	overriding
	function Should_Store( Property : in Password_Property_Type; Entity : in Entity_Type'Class ) return Boolean;
	-- The password should only be stored when changed



	overriding
	procedure Append_Create_Table( Property : in Password_Property_Type; Query : in out APQ.Root_Query_Type'Class );


	function New_Password_Property(
				Column_Name	: in     String;
				Getter		: Password_Getter_Callback;
				Setter		: Password_Setter_Callback;
				Immutable	: Boolean := False
			) return Entity_Property_Ptr;
	-- used to assist the creation of password properties.
	-- when Setter is NULL, KOW_view-entity_forms won't work for this property


end KOW_Ent.Properties;
