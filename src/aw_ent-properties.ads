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
-- Aw_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. Aw_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with Aw_Lib; see file COPYING. If not, write --
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
--
-- NOTE :: for now the only type implemented is Unbounded_String
-- TODO :: implement support for more types.



---------------
-- Ada Works --
---------------
with APQ;
with Aw_Ent;
with Aw_Lib.Locales;


package Aw_Ent.Properties is

	-----------------
	-- Foreign Key --
	-----------------
	
	-- this is used to map a foreign key for the entity
	type ID_Getter_Type is not null access function(
				Entity	: in Aw_Ent.Entity_Type'Class
			) return Aw_Ent.ID_Type;
	type ID_Setter_Type is not null access procedure(
				Entity	: in out Aw_Ent.Entity_Type'Class;
				ID	: in     Aw_Ent.ID_Type
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
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			);
	-- Set the property into the Entity.

	overriding
	procedure Get_Property(
				Property	: in     Foreign_Key_Property_Type;	-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			);


	function New_Foreign_Key_Property(
				Column_Name		: in String;
				Related_Entity_Tag	: in Ada.Tags.Tag;
				Getter			: in ID_Getter_Type;
				Setter			: in ID_Setter_Type
			) return Entity_Property_Ptr;


	---------------------
	-- Locale Property --
	---------------------
	type Locale_Getter_Type is not null access function(
				Entity : in Aw_ent.Entity_Type'Class
			) return Aw_Lib.Locales.Locale;
	type Locale_Setter_Type is not null access procedure(
				Entity : in out Aw_Ent.Entity_Type'Class;
				Locale : in     Aw_Lib.Locales.Locale
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
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			);
	-- Set the property into the Entity.

	overriding
	procedure Get_Property(
				Property	: in     Locale_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			);


	function New_Locale_Property(
				Column_Name		: in String;
				Getter			: in Locale_Getter_Type;
				Setter			: in Locale_Setter_Type
			) return Entity_Property_Ptr;




	-------------------------------
	-- Unbounded String property --
	-------------------------------

	type UString_Getter_Type is not null access function(
				Entity	: in Aw_Ent.Entity_Type'Class
			) return Unbounded_String;
	type UString_Setter_Type is not null access procedure(
				Entity	: in out Aw_Ent.Entity_Type'Class;
				Value	: in     Unbounded_String
			);

	type UString_Property_Type is new Entity_Property_Type with private;

	overriding
	procedure Set_Property(	
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			);
	-- Set the property into the Entity.

	overriding
	procedure Get_Property(
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			);


	function New_UString_Property(
				Column_Name	: in     String;
				Getter		: in     UString_Getter_Type;
				Setter		: in     UString_Setter_Type
			) return Entity_Property_Ptr;
	-- used to assist the creation of UString properties.
private
	-------------------------------
	-- Unbounded String property --
	-------------------------------

	type UString_Property_Type is new Entity_Property_Type with record
		Getter : UString_Getter_Type;
		Setter : UString_Setter_Type;
	end record;


end Aw_Ent.Properties;
