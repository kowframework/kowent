



with Ahven.Text_Runner;
with Ahven.Framework;

with KOW_Ent_Tests;



procedure Tester is
	S : Ahven.Framework.Test_Suite_Access := Ahven.Framework.Create_Suite( "KOW Framework :: Entities" );
begin

	Ahven.Framework.Add_Test( S.all, new KOW_Ent_Tests.Test );
	Ahven.Text_Runner.Run( S );
	Ahven.Framework.Release_Suite( S );
end Tester;
