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
with KOW_Ent;


---------
-- APQ --
---------
with APQ;


package body KOW_Ent.Properties is


	-----------------
	-- Foreign Key --
	-----------------

	overriding
	procedure Set_Property(	
				Property	: in     Foreign_Key_Property_Type;	-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			) is
		-- Set the property into the Entity.
		Column : String := To_String( Property.Column_Name );
		Index  : APQ.Column_Index_Type := APQ.Column_Index( Q, Column );
		Value  : APQ.APQ_Bigserial := KOW_Ent.ID_Value( Q, Index );
		Key    : KOW_Ent.Id_Type;
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
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			) is
		Key : KOW_Ent.ID_Type := Property.Getter.All( Entity );
	begin
		KOW_Ent.ID_Append( Query, Key.Value  );
	end Get_Property;



	overriding
	procedure Set_Property(
				Property	: in     Foreign_Key_Property_Type;	-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			) is
		-- Set the property from a String representation of the value
		Key : KOW_Ent.Id_Type;
	begin
		Key.Value  := APQ.APQ_Bigserial'Value( Value );
		Key.My_Tag := Property.Related_Entity_Tag;
		Property.Setter.all(
				Entity,
				Key
			);
	end Set_Property;
	
	overriding
	function Get_Property(
				Property	: in     Foreign_Key_Property_Type;	-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String is
		Value	: KOW_Ent.ID_Type := Property.Getter.All( Entity );
	begin
		return APQ.APQ_Bigserial'Image( Value.Value );
	end Get_Property;



	function New_Foreign_Key_Property(
				Column_Name		: in String;
				Related_Entity_Tag	: in Ada.Tags.Tag;
				Getter			: not null access function( Entity : in KOW_Ent.Entity_Type'Class ) return KOW_Ent.ID_Type;
				Setter			: not null access procedure( Entity : in out KOW_Ent.Entity_Type'Class; ID : in KOW_Ent.ID_Type )
			) return Entity_Property_Ptr is
		FK : Foreign_Key_property_Type;
	begin
		FK.Column_Name		:= To_Unbounded_String( Column_Name );
		FK.Related_Entity_Tag	:= Related_Entity_Tag;
		FK.Getter		:= Getter;
		FK.Setter		:= Setter;

		return new Foreign_Key_Property_Type'( FK );

	end New_Foreign_Key_Property;





	----------------------
	-- Boolean Property --
	----------------------

	overriding
	procedure Set_Property(	
				Property	: in     Boolean_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
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
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			) is
		Value : Boolean := Property.Getter.all( Entity );
	begin
		APQ.Append( Query, APQ.APQ_Boolean( Value ) );
	end Get_Property;


	overriding
	procedure Set_Property(
				Property	: in     Boolean_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			) is
		-- Set the property from a String representation of the value
		Bool_Value : Boolean := Boolean'Value( Value );
	begin
		Property.Setter.all(
				Entity,
				Bool_Value
			);
	end Set_Property;
	
	overriding
	function Get_Property(
				Property	: in     Boolean_Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String is
	begin
		return Boolean'Image( Property.Getter.all( Entity ) );
	end Get_Property;




	function New_Boolean_Property(
				Column_Name		: in String;
				Getter			: not null access function( Entity : in Entity_Type'Class ) return Boolean;
				Setter			: not null access procedure( Entity : in out Entity_Type'Class; Value : in Boolean )
			) return Entity_Property_Ptr is
		Bool : Boolean_Property_Type;
	begin
		Bool.Column_Name	:= To_Unbounded_String( Column_Name );
		Bool.Getter		:= Getter;
		Bool.Setter		:= Setter;
		return new Boolean_Property_Type'( Bool );
	end New_Boolean_Property;



	---------------------
	-- Locale Property --
	---------------------

	overriding
	procedure Set_Property(	
				Property	: in     Locale_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			) is
		-- Set the property into the Entity.
		Column		: String := To_String( Property.Column_Name );
		Index  		: APQ.Column_Index_Type := APQ.Column_Index( Q, Column );
		Locale_Code	: String := APQ.Value( Q, Index );
	begin
		Property.Setter.all(
				Entity,
				KOW_Lib.Locales.Get_Locale( To_Unbounded_String( Locale_Code ) )
			);

	end Set_Property;


	overriding
	procedure Get_Property(
				Property	: in     Locale_Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			) is
		Locale : KOW_Lib.Locales.Locale := Property.Getter.All( Entity );
	begin
		APQ.Append_Quoted( Query, Connection, To_String( Locale.CODE ) );
	end Get_Property;


	overriding
	procedure Set_Property(
				Property	: in     Locale_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			) is
		-- Set the property from a String representation of the value
	begin
		Property.Setter.all(
				Entity,
				KOW_Lib.Locales.Get_Locale( To_Unbounded_String( Value ) )
			);
	end Set_Property;
	
	overriding
	function Get_Property(
				Property	: in     Locale_Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String is
		Locale : KOW_Lib.Locales.Locale := Property.Getter.all( Entity );
	begin
		return To_String( Locale.CODE );
	end Get_Property;


	function New_Locale_Property(
				Column_Name		: in String;
				Getter			: not null access function( Entity : in Entity_Type'Class ) return KOW_Lib.Locales.Locale;
				Setter			: not null access procedure( Entity : in out Entity_Type'Class; Locale : in KOW_Lib.Locales.Locale )
			) return Entity_Property_Ptr is
		Loc : Locale_Property_Type;
	begin
		Loc.Column_Name	:= to_Unbounded_String( Column_Name );
		Loc.Getter	:= Getter;
		Loc.Setter	:= Setter;
		return new Locale_Property_Type'( Loc );
	end New_Locale_Property;


	-------------------------------
	-- Unbounded String property --
	-------------------------------
	procedure Set_Property(	
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
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
	exception
		when APQ.Null_Value =>
			Property.Setter.all(
				Entity,
				Property.Default_Value
			);
	end Set_Property;

	procedure Get_Property(
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			) is
	begin
		APQ.Append_Quoted( Query, Connection, To_String( Property.Getter.All( Entity ) ) );
	end Get_Property;

	overriding
	procedure Set_Property(
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			) is
		-- Set the property from a String representation of the value
	begin
		Property.Setter.all( Entity, To_Unbounded_String( Value ) );
	end Set_Property;
	
	overriding
	function Get_Property(
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String is
	begin
		return To_String( Property.Getter.all( Entity ) );
	end Get_Property;


	function New_UString_Property(
				Column_Name	: in     String;
				Getter		: not null access function( Entity : in Entity_Type'Class ) return Unbounded_String;
				Setter		: not null access procedure( Entity : in out Entity_Type'Class; Value : in Unbounded_String );
				Default_Value	: in     String := "N/A"
			) return Entity_Property_Ptr is
		-- used to assist the creation of UString properties.
		UStr : UString_Property_Type;
	begin
		UStr.Column_Name	:= To_Unbounded_String( Column_Name );
		UStr.Getter		:= Getter;
		UStr.Setter		:= Setter;
		UStr.Default_Value	:= To_Unbounded_String( Default_Value );
		return new UString_Property_Type'( UStr );
	end New_UString_Property;

	-----------------------
	-- Password Property --
	-----------------------
	
	overriding
	procedure Get_Property(
				Property	: in     Password_Property_Type;	-- the property worker
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_type'Class		-- the connection that belongs the query
			) is

		function Calculate_Hash return String is
			Str: Unbounded_String := Property.Getter.All( Entity );
		begin
			if Str = Null_Unbounded_String then
				return "";
			else
				return KOW_Ent.Calculate_Hash(
						To_String( Str )
					);
			end if;
		end Calculate_Hash;


		Hash : String := Calculate_Hash;
	begin
		if Hash /= "" then
			APQ.Append_Quoted( Query, Connection, Calculate_Hash );
		end if;
	end Get_Property;


	overriding
	procedure Set_Property(
				Property	: in     Password_Property_Type;	-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			) is
		-- Set the property from a String representation of the value
	begin
		if Property.Setter /= null then
			Property.Setter.all( Entity, To_Unbounded_String( Value ) );
		end if;
	end Set_Property;
	
	overriding
	function Get_Property(
				Property	: in     Password_Property_Type;	-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String is
	begin
		return To_String( Property.Getter.all( Entity ) );
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
				Getter		: in     Password_Getter_Type;
				Setter		: in     Password_Setter_Type := null
			) return Entity_Property_Ptr is
		-- used to assist the creation of password properties.
	begin
		return new Password_Property_Type'(
				Column_Name	=> To_Unbounded_String( Column_Name ),
				Getter		=> Getter,
				Setter		=> Setter
			);
	end New_Password_Property;

end KOW_Ent.Properties;
