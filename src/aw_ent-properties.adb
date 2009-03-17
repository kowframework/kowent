------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Entity                             --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                  --
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


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Hash;

---------------
-- Ada Works --
---------------
with Aw_Ent;


---------
-- APQ --
---------
with APQ;


package body Aw_Ent.Properties is


	-----------------
	-- Foreign Key --
	-----------------

	overriding
	procedure Set_Property(	
				Property	: in     Foreign_Key_Property_Type;	-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			) is
		-- Set the property into the Entity.
		Column : String := To_String( Property.Column_Name );
		Index  : APQ.Column_Index_Type := APQ.Column_Index( Q, Column );
		Value  : APQ.APQ_Bigserial := Aw_Ent.ID_Value( Q, Index );
		Key    : Aw_Ent.Id_Type;
	begin
		Key.Value := Value;
		Key.My_Tag := Property.Related_Entity_Tag;
		Property.Setter.all(
				Entity,
				Key
			);

	end Set_Property;

	overriding
	procedure Get_Property(
				Property	: in     Foreign_Key_Property_Type;	-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			) is
		Key : Aw_Ent.ID_Type := Property.Getter.All( Entity );
	begin
		Aw_Ent.ID_Append( Query, Key.Value  );
	end Get_Property;


	function New_Foreign_Key_Property(
				Column_Name		: in String;
				Related_Entity_Tag	: in Ada.Tags.Tag;
				Getter			: in ID_Getter_Type;
				Setter			: in ID_Setter_Type
			) return Entity_Property_Ptr is
	begin
		return new Foreign_Key_Property_Type'(
				Column_Name		=> To_Unbounded_String( Column_Name ),
				Related_Entity_Tag	=> Related_Entity_Tag,
				Getter			=> Getter,
				Setter			=> Setter
				);

	end New_Foreign_Key_Property;





	----------------------
	-- Boolean Property --
	----------------------

	overriding
	procedure Set_Property(	
				Property	: in     Boolean_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			) is
		-- Set the property into the Entity.
		Index	: APQ.Column_Index_Type := APQ.Column_Index( Q, To_String( Property.Column_Name ) );
		Value	: Boolean := APQ.Value( Q, Index );
	begin
		Property.Setter.all(
				Entity,
				Value
			);
	end Set_Property;

	overriding
	procedure Get_Property(
				Property	: in     Boolean_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			) is
		Value : Boolean := Property.Getter.all( Entity );
	begin
		APQ.Append( Query, APQ.APQ_Boolean( Value ) );
	end Get_Property;


	function New_Boolean_Property(
				Column_Name		: in String;
				Getter			: in Boolean_Getter_Type;
				Setter			: in Boolean_Setter_Type
			) return Entity_Property_Ptr is
	begin
		return new Boolean_Property_Type'(
				Column_Name	=> To_Unbounded_String( Column_Name ),
				Getter		=> Getter,
				Setter		=> Setter
			);
	end New_Boolean_Property;



	---------------------
	-- Locale Property --
	---------------------

	overriding
	procedure Set_Property(	
				Property	: in     Locale_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			) is
		-- Set the property into the Entity.
		Column		: String := To_String( Property.Column_Name );
		Index  		: APQ.Column_Index_Type := APQ.Column_Index( Q, Column );
		Locale_Code	: String := APQ.Value( Q, Index );
	begin
		Property.Setter.all(
				Entity,
				Aw_Lib.Locales.Get_Locale( To_Unbounded_String( Locale_Code ) )
			);

	end Set_Property;


	overriding
	procedure Get_Property(
				Property	: in     Locale_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			) is
		Locale : Aw_Lib.Locales.Locale := Property.Getter.All( Entity );
	begin
		APQ.Append_Quoted( Query, Connection.all, To_String( Locale.CODE ) );
	end Get_Property;

	function New_Locale_Property(
				Column_Name		: in String;
				Getter			: in Locale_Getter_Type;
				Setter			: in Locale_Setter_Type
			) return Entity_Property_Ptr is
	begin
		return new Locale_Property_Type'(
				Column_Name	=> To_Unbounded_String( Column_Name ),
				Getter		=> Getter,
				Setter		=> Setter
			);
	end New_Locale_Property;


	-------------------------------
	-- Unbounded String property --
	-------------------------------
	procedure Set_Property(	
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			) is
		-- Set the property into the Entity.

		Column : String := To_String( Property.Column_Name );
		Index  : APQ.Column_Index_Type := APQ.Column_Index( Q, Column );
		Value  : String := APQ.Value( Q, Index );
	begin
		Property.Setter.all(
				Entity,
				To_Unbounded_String( Value )
			);
	end Set_Property;

	procedure Get_Property(
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			) is
	begin
		APQ.Append_Quoted( Query, Connection.all, To_String( Property.Getter.All( Entity ) ) );
	end Get_Property;


	function New_UString_Property(
				Column_Name	: in     String;
				Getter		: in     UString_Getter_Type;
				Setter		: in     UString_Setter_Type
			) return Entity_Property_Ptr is
		-- used to assist the creation of UString properties.
	begin
		return new UString_Property_Type'(
				Column_Name	=> To_Unbounded_String( Column_Name ),
				Getter		=> Getter,
				Setter		=> Setter
				);
	end New_UString_Property;

	-----------------------
	-- Password Property --
	-----------------------
	
	overriding
	procedure Get_Property(
				Property	: in     Password_Property_Type;	-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			) is

		function Calculate_Hash return String is
			Str: Unbounded_String := Property.Getter.All( Entity );
		begin
			if Str = Null_Unbounded_String then
				return "";
			else
				return Aw_Ent.Calculate_Hash(
						To_String( Str )
					);
			end if;
		end Calculate_Hash;


		Hash : String := Calculate_Hash;
	begin
		if Hash /= "" then
			APQ.Append_Quoted( Query, Connection.all, Calculate_Hash );
		end if;
	end Get_Property;

	overriding
	function Should_Read( Property : in Password_Property_Type ) return Boolean is
		-- The password should never be read from the database, so this is == false
	begin
		return False;
	end Should_Read;

	overriding
	function Should_Store( Property : in Password_Property_Type; Entity : in Entity_Type'Class ) return Boolean is
		-- The password should only be stored when changed
	begin
		return Property.Getter.All( Entity ) /= Null_Unbounded_String;
	end Should_Store;




	function New_Password_Property(
				Column_Name	: in     String;
				Getter		: in     Password_Getter_Type
			) return Entity_Property_Ptr is
		-- used to assist the creation of password properties.
	begin
		return new Password_Property_Type'(
				Column_Name	=> To_Unbounded_String( Column_Name ),
				Getter		=> Getter
			);
	end New_Password_Property;

end Aw_Ent.Properties;
