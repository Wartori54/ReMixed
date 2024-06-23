// Compile with:
// ilasm /dll /output:../bin/Debug/net7.0/ILTestMethods.dll TestMethods.il

.assembly TestMethods
{
    .ver 0:0:0:0
}

.class public auto ansi beforefieldinit ReMixed.Tests.ILClass
    extends [System.Private.CoreLib]System.Object
{
    .method public hidebysig 
        instance void FancyBr(int32 val, int32 other) cil managed 
    {
        .maxstack 8
        
        ldarg val
        ldarg other
        beq Exit
        ldstr "equal"
        call void [System.Console]System.Console::WriteLine(string)
        ldc.i4.0
        brfalse Exit
        Exit:
        ret
    }

    .method public hidebysig specialname rtspecialname 
        instance void .ctor () cil managed 
    {
        .maxstack 8

        IL_0000: ldarg.0
        IL_0001: call instance void [System.Private.CoreLib]System.Object::.ctor()
        IL_0006: ret
    }

}

