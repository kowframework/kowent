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

------------------------------------------------------------------------------
-- Usefull property types for KOW Ent                                       --
------------------------------------------------------------------------------



-------------------
-- KOW Framework --
-------------------
with KOW_Ent.Properties;
with KOW_Lib.Locales;


package KOW_Ent.Extra_Properties is



	--------------------------
	-- Locale Property Type --
	--------------------------

	type Locale_Property_Type(
				Name		: Property_Name_Type;
				Container	: Property_Container_Ptr
			) is new KOW_Ent.Properties.Valued_Property_Type(
							Name		=> Name,
							Container	=> Container,
							Type_Of		=> APQ_String,
							String_Length	=> 5,
							Allow_Null	=> False
						) with null record;
	overriding
	procedure Initialize( Property : in out Locale_Property_Type );
	-- initialize the locale as the default locale
	
	procedure Set_Locale_Code(
				Property	: in out Locale_Property_Type;
				Locale_Code	: in     KOW_Lib.Locales.Locale_Code_Type
			);
	-- set the locale
	
	function Get_Locale(
				Property	: in     Locale_Property_Type
			) return KOW_Lib.Locales.Locale_Type;
	-- it's an alias for KOW_Lib.Locales.Get_Locale( Get_locale_Code (Property ));
	
	function Get_Locale_Code(
				Property	: in     Locale_Property_Type
			) return KOW_Lib.Locales.Locale_Code_Type;
	


	----------------------------
	-- Password Property Type --
	----------------------------

	type Password_Property_Type(
				Name		: Property_Name_type;
				Container	: Property_Container_Ptr
			) is new KOW_Ent.Properties.Valued_Property_Type(
							Name		=> Name,
						       	Container	=> Container,
							Type_Of		=> APQ_String,
							String_Length	=> 40,
						       	Allow_NUll	=> False
					     ) with null record;
	-- holds a SHA1 hashed passowrd

	procedure Set_Password(
				Property	: in out Password_Property_Type;
				Password	: in     String
			);
	-- this is the code that will encode the password as SHA1.


	function SHA1_Hash( Value : in String ) return String;
	-- compute the SHA1 hash using the GNAT packages
	
	function Password_Value( Password : in String ) return Value_Type;
	-- this will return a value type with the password encoded as a SHA1 hash
	-- use this function to generate the value to match against the stored value




end KOW_Ent.Extra_Properties;
