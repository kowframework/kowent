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
	
	-- this is used to map a foreign key for the entity
	type ID_Getter_Type is not null access function(
				Entity	: in KOW_Ent.Entity_Type'Class
			) return KOW_Ent.ID_Type;
	type ID_Setter_Type is not null access procedure(
				Entity	: in out KOW_Ent.Entity_Type'Class;
				ID	: in     KOW_Ent.ID_Type
			);
	
	type Foreign_Key_Property_Type is new Entity_Property_Type with record
		Related_Entity_Tag	: Ada.Tags.Tag;
		Getter			: ID_Getter_Type;
		Setter			: ID_Setter_Type;
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




	function New_Foreign_Key_Property(
				Column_Name		: in String;
				Related_Entity_Tag	: in Ada.Tags.Tag;
				Getter			: in ID_Getter_Type;
				Setter			: in ID_Setter_Type
			) return Entity_Property_Ptr;


	----------------------
	-- Boolean Property --
	----------------------

	type Boolean_Getter_Type is not null access function(
				Entity	: in KOW_Ent.Entity_Type'Class
			) return Boolean;
	type Boolean_Setter_Type is not null access procedure(
				Entity	: in out KOW_Ent.Entity_Type'Class;
				ID	: in     Boolean
			);
	
	type Boolean_Property_Type is new Entity_Property_Type with record
		Getter			: Boolean_Getter_Type;
		Setter			: Boolean_Setter_Type;
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



	function New_Boolean_Property(
				Column_Name		: in String;
				Getter			: in Boolean_Getter_Type;
				Setter			: in Boolean_Setter_Type
			) return Entity_Property_Ptr;



	---------------------
	-- Locale Property --
	---------------------
	type Locale_Getter_Type is not null access function(
				Entity : in KOW_ent.Entity_Type'Class
			) return KOW_Lib.Locales.Locale;
	type Locale_Setter_Type is not null access procedure(
				Entity : in out KOW_Ent.Entity_Type'Class;
				Locale : in     KOW_Lib.Locales.Locale
			);



	type Locale_Property_Type is new Entity_Property_Type with record
		Getter			: Locale_Getter_Type;
		Setter			: Locale_Setter_Type;
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




	function New_Locale_Property(
				Column_Name		: in String;
				Getter			: in Locale_Getter_Type;
				Setter			: in Locale_Setter_Type
			) return Entity_Property_Ptr;




	-------------------------------
	-- Unbounded String property --
	-------------------------------

	type UString_Getter_Type is not null access function(
				Entity	: in KOW_Ent.Entity_Type'Class
			) return Unbounded_String;
	type UString_Setter_Type is not null access procedure(
				Entity	: in out KOW_Ent.Entity_Type'Class;
				Value	: in     Unbounded_String
			);

	type UString_Property_Type is new Entity_Property_Type with record
		Getter		: UString_Getter_Type;
		Setter		: UString_Setter_Type;
		Default_Value	: Unbounded_String;
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



	function New_UString_Property(
				Column_Name	: in     String;
				Getter		: in     UString_Getter_Type;
				Setter		: in     UString_Setter_Type;
				Default_Value	: in     String := "N/A"
			) return Entity_Property_Ptr;
	-- used to assist the creation of UString properties.
	-- default_value represents the value to be set when the one retoner from database is NULL


	-----------------------
	-- Password Property --
	-----------------------

	type Password_Getter_Type is not null access function(
				Entity	: in KOW_Ent.Entity_Type'Class
			) return Unbounded_String;

	type Password_Setter_Type is access procedure(
				Entity	: in out KOW_Ent.Entity_Type'Class;
				Password: in     Unbounded_String
			);
	-- NOTE:: the password setter type CAN be null!
		

	type Password_Property_Type is new Entity_Property_Type with record
		-- A password property is a property that never reads from the database
		-- it's only used to set a new password.
		--
		-- The stored password is Hashed.
		Getter : Password_Getter_Type;
		-- get the non-hashed password


		Setter : Password_Setter_Type;
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


	function New_Password_Property(
				Column_Name	: in     String;
				Getter		: in     Password_Getter_Type;
				Setter		: in     Password_Setter_Type := null
			) return Entity_Property_Ptr;
	-- used to assist the creation of password properties.
	-- when Setter is NULL, KOW_view-entity_forms won't work for this property



	-------------------
	-- File Location --
	-------------------

--	NOTE: for now I'm gonna implement file upload using pure Ada structures with 1 file from DB entry
--	-- TODO :: desenhar a parte de file upload e IMPLEMENTAR
--	type File_Location_Property is new UString_Property with record
--		Root_Path : Unbounded_String;
--		-- where the files should be stored
--		--
--		-- on the KOW framework files aren't stored in the database backend.
--		-- Even though storing in the database is easier and more portable
--		-- storing on the file system should be easier to handle.
--		--
--		-- More than that, there are also default getter and setter.
--
--		Default_Extension : Unbounded_String;
--	end record;

end KOW_Ent.Properties;
