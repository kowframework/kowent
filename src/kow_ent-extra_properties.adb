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
-- Usefull property types for KOW Ent                                       --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

----------
-- GNAT --
----------
with GNAT.SHA1;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Locales;
with KOW_Lib.String_Util;

package body KOW_Ent.Extra_Properties is


	--------------------------
	-- Locale Property Type --
	--------------------------

	overriding
	procedure Initialize( Property : in out Locale_Property_Type ) is
		-- initialize the locale as the default locale
	begin
		Set_Locale_Code( Property, KOW_Lib.Locales.Get_Default_Locale_Code );
		-- set the locale

		KOW_Ent.Initialize( Property_Type( Property ) );
		-- run the standard registration code
	end Initialize;

	procedure Set_Locale_Code(
				Property	: in out Locale_Property_Type;
				Locale_Code	: in     KOW_Lib.Locales.Locale_Code_Type
			) is
	begin
		KOW_Lib.String_Util.Copy(
						From	=> KOW_Lib.Locales.To_String( Locale_Code ),
						To	=> Property.Value.String_Value
					);
	end Set_Locale_Code;
	-- set the locale
	
	function Get_Locale(
				Property	: in     Locale_Property_Type
			) return KOW_Lib.Locales.Locale_Type is
		-- it's an alias for KOW_Lib.Locales.Get_Locale( Get_locale_Code (Property ));
	begin
		return KOW_Lib.Locales.Get_Locale( Get_Locale_Code( Property ) );
	end Get_Locale;

	function Get_Locale_Code(
				Property	: in     Locale_Property_Type
			) return KOW_Lib.Locales.Locale_Code_Type is
	begin
		return KOW_Lib.Locales.From_String( Ada.Strings.Fixed.Trim( Property.Value.String_Value, Ada.Strings.Right ) );
	end Get_Locale_Code;


	----------------------------
	-- Password Property Type --
	----------------------------

	procedure Set_Password(
				Property	: in out Password_Property_Type;
				Password	: in     String
			) is
	begin
		-- this is the code that will encode the password as SHA1.
		Property.Value := Password_Value( Password );
	end Set_Password;

	function SHA1_Hash( Value : in String ) return String is
		-- compute the SHA1 hash using the GNAT packages
		use GNAT.SHA1;
		C : Context;
	begin
		Update( C, Value );
		return Digest( C );
	end SHA1_Hash;

	
	function Password_Value( Password : in String ) return Value_Type is
		-- this will return a value type with the password encoded as a SHA1 hash
		-- use this function to generate the value to match against the stored value
		V : Value_Type( APQ_String, 40 );
	begin
		V.String_Value := SHA1_Hash( Password );
		return V;
	end Password_Value;

end KOW_Ent.Extra_Properties;
