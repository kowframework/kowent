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





--------------
-- Ada 2005 --
--------------
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Tags;
with Ada.Text_IO;	use Ada.Text_IO;


-------------------
-- KOW Framework --
-------------------
with APQ;
with KOW_Ent;				use KOW_Ent;
with KOW_Ent.Properties;
with KOW_Lib.Locales;
with KOW_Lib.Calendar;


package body KOW_Ent.Extra_Properties is

	----------------------
	-- The Percent Type --
	----------------------

	function To_String( P : in Percent ) return String is
	begin
		return Percent'Image( P );
	end To_String;
	function From_String( Str_P : in String ) return Percent is
	begin
		return Percent'Value( Str_P );
	end From_String;



	function To_String( M : in Money ) return String is
	begin
		return Money'Image( M );
	end To_String;



	function From_String( Str_M : in String ) return Money is
	begin
		return Money'Value( Str_M );
	end From_String;


	-------------------
	-- The Date Type --
	-------------------

	function To_String( D : in Date ) return String is
		-- format using YYYY-MM-DD
		-- TODO :: reimplentar usando KOW_Lib.Calendar.Format
		use Ada.Calendar;
		use Ada.Strings;
		use Ada.Strings.Fixed;

		function T( Str : in String ) return String is
		begin
			if Str'Length = 1 then
				return '0' & Str;
			else
				return Str;
			end if;
		end T;

		Year_Str 	: String := Trim( Year_Number'Image( Year( D ) ), Both );
		Month_Str	: String := T( Trim( Month_Number'Image( Month( D ) ), Both ) );
		Day_Str		: String := T( Trim( Day_Number'Image( Day( D ) ), Both ) );

	begin
		-- return KOW_Lib.Calendar.Format( D );

		return Year_Str & '-' & Month_Str & '-' & Day_Str;
	end To_String;

	
	function From_String( Str_D : in String ) return Date is
		-- parse a string in the form YYYY-MM-DD
		-- simply assume it's in the right format and try parsing it... :D
		-- it's a little bit naieve, but works
		-- TODO :: reimplementar usando KOW_Lib.Calendar.Parse( Str_D );
		-- NOTE :: KOW_Lib.Calendar.Parse( Str_D ) precisa ser implementada
		Year	: Ada.Calendar.Year_Number;
		Month	: Ada.Calendar.Month_Number;
		Day	: Ada.Calendar.Day_Number;
		F	: Integer := Str_D'First;
	begin
		Year	:= Ada.Calendar.Year_Number'Value( Str_D( F .. F + 3 ) );
		Month	:= Ada.Calendar.Month_Number'Value( Str_D( F + 5 .. F + 6 ) );
		Day	:= Ada.Calendar.Day_Number'Value( Str_D( F + 8 .. F + 9 ) );
		return Ada.Calendar.Time_of( Year, Month, Day );
	exception

--		return KOW_Lib.Calendar.Parse( D );
		when CONSTRAINT_ERROR =>
			raise CONSTRAINT_ERROR with "date """ & Str_D & """ not in the expected format ""YYYY-MM-DD""";
	end From_String;



	------------------------
	-- The Timestamp Type --
	------------------------
	

	function Timestamp_To_String( D : in Timestamp ) return String is
	begin
		return APQ.To_String( APQ.APQ_Timestamp( D ) );
	end Timestamp_to_String;


	function Timestamp_From_String( Str_D : in String ) return Timestamp is
		use Ada.Calendar;


		function To_Timestamp is new APQ.Convert_To_Timestamp( Timestamp );
	begin
		return To_Timestamp( Str_D, 0 );
	end Timestamp_From_String;


	------------------------
	-- The Dimension Type --
	------------------------

	function Dimension_To_String( D : in Dimension ) return String is
	begin
		return Dimension'Image( D );
	end Dimension_To_String;

	function Dimension_From_String( Str_D : in String ) return Dimension is
	begin
		return Dimension'Value( Str_D );
	end Dimension_From_String;


	---------------------
	-- The Weight Type --
	---------------------

	function Weight_To_String( W : in Weight ) return String is
	begin
		return Weight'Image( W );
	end Weight_To_String;


	function Weight_From_String( Str_W : in String ) return Weight is
	begin
		return Weight'Value( Str_W );
	end Weight_From_String;



	--------------------
	-- The Count Type --
	--------------------

	function Count_To_String( C : in Count ) return String is
	begin
		return Count'Image( C );
	end Count_To_String;

	function Count_From_String( Str_C : in String ) return Count is
	begin
		return Count'Value( Str_C );
	end Count_From_String;


	-----------------------------------
	-- Operações com Percent e Money --
	-----------------------------------

	package lol is
		type My_MOney is new Money;
	end lol;
	function "*"( L : in Money; R : in Percent ) return Money is
		-- calcula porcentagem do dinheiro

		use lol;
	begin
		-- TODO : see why there is no * for the Money type
		return Money(
				My_Money( L ) * My_Money( R )
			);
	end "*";

	function "+"( L : in Money; R : in Percent ) return Money is
		-- calcula L + ( R * L ) )
	begin
		return L + ( L * R );
	end "+";



	function "-"( L : in Money; R : in Percent ) return Money is
		-- calcula L + ( R * L ) )
	begin
		return L - ( L * R );
	end "-";



	--------------------
	-- The Tag Entity --
	--------------------

	procedure Tag_It( Entity : in KOW_Ent.Entity_Type'Class; Value : in String ) is
	begin
		Tag_It( Entity, To_Unbounded_String( Value ) );
	end Tag_It;

	procedure Tag_It( Entity : in KOW_Ent.Entity_Type'Class; Value : in Unbounded_String ) is
		The_Tag : Tag_Entity;
		use KOW_Ent;
		use Ada.Tags;
	begin
		if Entity.Id.My_Tag = No_Tag then
			raise CONSTRAINT_ERROR with "can't tag an entity that hasn't been saved already";
		end if;
		The_Tag.Value		:= Value;
		The_Tag.Entity_Id	:= Entity.Id;
		The_Tag.Entity_Tag	:= To_Unbounded_String( Expanded_Name( Entity'Tag ) );

		Store( The_Tag );
	end Tag_It;







	--
	-- entity id
	--


	procedure Set_T_Entity_id( E : in out Entity_Type'Class; ID : in ID_Type ) is
	begin
		Tag_Entity( E ).Entity_ID := ID;
	end Set_T_Entity_ID;

	function Get_T_Entity_Id( E : in Entity_Type'Class ) return ID_Type is
	begin
		return Tag_Entity( E ).Entity_Id;
	end Get_T_Entity_Id;


	--
	-- value
	--

	procedure Set_T_Value( E : in out Entity_Type'Class; Value : in Unbounded_String ) is
	begin
		Tag_Entity( E ).Value := Value;
	end Set_T_Value;

	function Get_T_Value( E : in Entity_Type'CLass ) return Unbounded_String is
	begin
		return Tag_Entity( E ).Value;
	end Get_T_Value;


	--
	-- entity tag
	--
	procedure Set_T_Entity_Tag( E : in out Entity_Type'Class; Entity_Tag : in Unbounded_String ) is
	begin
		Tag_Entity( E ).Entity_Tag := Entity_Tag;
	end Set_T_Entity_Tag;


	function Get_T_Entity_Tag( E : in Entity_Type'Class ) return Unbounded_String is
	begin
		return Tag_Entity( E ).Entity_Tag;
	end Get_T_Entity_Tag;




begin
	
	KOW_Ent.Entity_Registry.Register(
			Entity_Tag	=> Tag_Entity'Tag,
			Table_Name	=> "tags",
			Id_Generator	=> null,
			Factory		=> null
		);

	KOW_Ent.Entity_Registry.Add_Property(
			Entity_Tag	=> Tag_Entity'Tag,
			Property	=> KOW_Ent.Properties.NEw_Foreign_Key_Property(
							Column_Name		=> "entity_id",
							Related_Entity_Tag	=> Entity_Type'Tag,		-- placeholder..
							Getter			=> Get_T_Entity_Id'Access,
							Setter			=> Set_T_Entity_Id'Access
						)
		);
	
	KOW_Ent.Entity_Registry.Add_Property(
			Entity_Tag	=> Tag_Entity'Tag,
			Property	=> KOW_Ent.Properties.New_UString_Property(
							Column_Name	=> "value",
							Getter		=> Get_T_Value'Access,
							Setter		=> Set_T_Value'Access
						)
		);

	KOW_Ent.Entity_Registry.Add_Property(
			Entity_Tag	=> Tag_Entity'Tag,
			Property	=> KOW_Ent.Properties.New_UString_Property(
							Column_Name	=> "entity_tag",
							Getter		=> Get_T_Entity_Tag'Access,
							Setter		=> Set_T_Entity_Tag'Access
						)
		);






end KOW_Ent.Extra_Properties;
