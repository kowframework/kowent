------------------------------------------------------------------------------
--                                                                          --
--                       KOW Framework :: Entities                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
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

------------------------------------------------------------------------------
-- Main KOW_Ent package.                                                    --
--                                                                          --
-- KOW_Ent is reponsible for handling persistent data in your application   --
-- stored in Database backends using the native DB types.                   --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with APQ;
with KOW_Ent.Data_Storages;
with KOW_Lib.String_Util;

package body KOW_Ent is



	--------------------
	-- The Value Type --
	--------------------

	function To_String( Value : in Value_Type ) return String is
		-- convert the value into a not trimmed string
	begin
		case Value.Type_Of is
			when APQ_Smallint =>
				return APQ.APQ_Smallint'Image( Value.Smallint_Value );

			when APQ_Integer =>
				return APQ.APQ_Integer'Image( Value.Integer_Value );

			when APQ_Bigint =>
				return APQ.APQ_Bigint'Image( Value.Bigint_Value );

			when APQ_Real	=>
				return APQ.APQ_Real'Image( Value.Real_Value );

			when APQ_Double =>
				return APQ.APQ_Double'Image( Value.Double_Value );

			when APQ_Serial	=>
				return APQ.APQ_Serial'Image( Value.Serial_Value );

			when APQ_Bigserial =>
				return APQ.APQ_Bigserial'Image( Value.Bigserial_Value );

			when APQ_Date =>
				-- TODO :: convert to string
				return "";

			when APQ_Time =>
				-- TODO :: convert to string
				return "";

			when APQ_Timestamp =>
				-- TODO :: convert to string
				return "";


			when Hour_Number =>
				return APQ.Hour_Number'Image( Value.Hour_Value );
			
			when Minute_Number =>
				return APQ.Minute_Number'Image( Value.Minute_Value );

			when Second_Number =>
				return APQ.Second_Number'Image( Value.Second_Value );

			when APQ_String =>
				return Value.String_Value;
		end case;
	end To_String;




	procedure From_String( Value : in out Value_Type; String_Value : in String ) is
		-- set the value form a string
		-- if the value.type_of=apq_string, raises constraint_error only if string_value'length > value.string_value'length
	begin
		case Value.Type_Of is
			when APQ_Smallint =>
				Value.Smallint_Value := APQ.APQ_Smallint'Value( String_Value );

			when APQ_Integer =>
				Value.Integer_Value := APQ.APQ_Integer'Value( String_Value );

			when APQ_Bigint =>
				Value.Bigint_Value := APQ.APQ_Bigint'Value( String_Value );

			when APQ_Real	=>
				Value.Real_Value := APQ.APQ_Real'Value( String_Value );

			when APQ_Double =>
				Value.Double_Value := APQ.APQ_Double'Value( String_Value );

			when APQ_Serial	=>
				Value.Serial_Value := APQ.APQ_Serial'Value( String_Value );

			when APQ_Bigserial =>
				Value.Bigserial_Value := APQ.APQ_Bigserial'Value( String_Value );

			when APQ_Date =>
				-- TODO :: convert from string
				null;
			when APQ_Time =>
				-- TODO :: convert from string
				null;

			when APQ_Timestamp =>
				-- TODO :: convert from string
				null;


			when Hour_Number =>
				Value.Hour_Value := APQ.Hour_Number'Value( String_Value );
			
			when Minute_Number =>
				Value.Minute_Value := APQ.Minute_Number'Value( String_Value );

			when Second_Number =>
				Value.Second_Value := APQ.Second_Number'Value( String_Value );

			when APQ_String =>
				KOW_Lib.String_Util.Copy( To => Value.String_Value, From => String_Value );
		end case;
	end From_String;




	-----------------------
	-- The Property Type --
	-----------------------



	function PN( Name : in String ) return Property_Name_Type is
	begin
		return new String'( Name );
	end PN;


	overriding
	procedure Initialize( Property : in out Property_Type ) is
		-- register the given property in the given container
	begin
		Register( Property.Container.all, Property'Unrestricted_Access );
	end Initialize;

	function Ignore_For_Insert( Property : in Property_Type ) return Boolean is
		-- should the framework ignore this property in insert queries?
	begin
		return false;
	end Ignore_For_Insert;

	function Ignore_For_Update( Porperty : in Property_Type ) return Boolean is
		-- should the framework ignore this property in update queries?
	begin
		return false;
	end Ignore_For_Update;


	function Is_Id( Property : in Property_Type ) return Boolean is
		-- if true, when updating use this to index the value
	begin
		return False;
	end Is_Id;

	---------------------------------
	-- The Property Container Type --
	---------------------------------


	procedure Clone(
			From	: in     Property_Container_Type;
			To	: in out Property_Container_Type'Class
		) is
		-- clone the data from [From] to [To]
		-- the cloning procedure copy only values from registered properties
		use Ada.Tags;
		use Property_Lists;

		C_From, C_To : Cursor;
		P_From, P_To : Property_Ptr;
	begin
		pragma Assert( Property_Container_Type'Class( From )'Tag = To'Tag, "I can only clone between elements of the same type");

		if Is_Empty( From.Properties ) then
			-- nothing to do...
			return;
		end if;


		loop
			C_From := First( From.Properties );
			C_To := First( To.Properties );
			exit when not Has_Element( C_From );

			pragma Assert( Has_Element( C_To ), "some weird problem with ammount of properties in the container... you should check your code and don't call Register in the container yourself" );

			P_From := Element( C_From );
			P_TO := Element( C_To );
			Set_Value( P_To.all, Get_Value( P_From.all ) );
		end loop;
	end Clone;

	procedure Register(
			Container	: in out Property_Container_Type;
			Property	: in     Property_Ptr
		) is
		-- register a property into this property container
		-- this is to be called when the property is allocated
	begin
		Property_Lists.Append( Container.Properties, Property );
	end Register;
	
	procedure Iterate(
			Container	: in out Property_Container_Type;
			Iterator	: access procedure( Property : Property_Ptr )
		) is
		-- iterate over all registered properties in this container
		procedure Inner_iterator( C : Property_Lists.Cursor ) is
		begin
			Iterator.all( Property_Lists.Element( C ) );
		end Inner_iterator;
	begin
		Property_Lists.Iterate( Container.Properties, Inner_Iterator'Access );
	end Iterate;


	---------------------------
	-- The Entity Alias Type --
	---------------------------

	function To_Alias( Alias_String : in String ) return Entity_Alias_Type is
		-- convenient way for converting an alias string (any length) into an entity alias string
		Alias : Entity_Alias_Type;
	begin
		KOW_Lib.String_Util.Copy( From => Alias_String, To => String( Alias ) );
		return Alias;
	end To_Alias;


	function Trim( Alias : in Entity_Alias_Type ) return String is
		-- convenient way for returning the trimmed version of the alias
	begin
		return Ada.Strings.Fixed.Trim(
					String( Alias ),
					Ada.Strings.Right
				);
	end Trim;


	---------------------
	-- The Entity Type --
	---------------------


	function Version_Of( Entity : in Entity_Type ) return String is
	begin
		return "-1";
	end Version_Of;

	procedure Store( Entity : in out Entity_Type ) is
		-- procedure used to store the entity;
		-- for retrieving please read the documentation of the used data storage
	begin
		if Entity.Data_Storage /= null and then Entity.Data_Storage.all in Data_Storages.Data_Storage_Type'Class then
			Data_Storages.Update(
					Data_Storages.Data_Storage_Type'Class( Entity.Data_Storage.all ),
					Entity,
					Get_Id( Entity_Type'Class( Entity ) )
				);
		else
			Data_Storages.Insert(
					Data_Storages.Data_Storage_Type'Class( Data_Storages.Get_Data_Storage( Entity_Type'Class( Entity )'Tag ).all ),
					Entity
				);
		end if;
	end Store;



	function Get_Alias( Entity : in Entity_Type ) return Entity_Alias_Type is
	begin
		if Entity.Data_Storage /= null and then Entity.Data_Storage.all in Data_Storages.Data_Storage_Type'Class then
			return Data_Storages.Get_Alias(
					Data_Storages.Data_Storage_Type'Class( Entity.Data_Storage.all ),
					Entity_Type'Class( Entity )'Tag
				);
		else
			return Data_Storages.Get_Alias(
					Data_Storages.Data_Storage_Type'Class( Data_Storages.Get_Data_Storage( Entity_Type'Class( Entity )'tag).all ),
					Entity_Type'Class( Entity )'Tag
				);
		end if;

	exception
		when CONSTRAINT_ERROR =>
			raise PROGRAM_ERROR with "don't know how to get the alias for the entity " & Ada.Tags.Expanded_Name( Entity_Type'Class( Entity )'Tag );

	end Get_Alias;

	function Get_id( Entity : in Entity_Type ) return Property_Type'Class is
		-- get the ID property
		-- the default implementation look for elements where Is_Id( Property ) = true
		-- if none found, raises constraint error with an informative message
		ID : Property_Ptr := null;
		procedure Iterator( C : in Property_Lists.Cursor ) is
			Prop : Property_Ptr := Property_Lists.Element( C );
		begin
			if Is_Id( Prop.all ) then
				ID := Prop;
			end if;
		end Iterator;
	begin
		Property_Lists.Iterate( Entity.Properties, Iterator'Access );
		if ID = null then
			raise CONSTRAINT_ERROR with "there is no ID property in this entity: " & Ada.Tags.Expanded_Name( Entity_Type'Class( Entity )'tag );
		end if;

		return ID.All;
	end Get_Id;

end KOW_Ent;
