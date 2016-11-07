(***************************************************************************)
(*                                                                         *)
(*                     Copyright (C) 2002-2004                             *)
(*                        by Stony Brook Software                          *)
(*                                                                         *)
(*                          All rights reserved.                           *)
(*                                                                         *)
(***************************************************************************)
IMPLEMENTATION MODULE Blowfish;
<*/OPTIMIZE:T*>

FROM SYSTEM IMPORT
    BYTE, ADDRESS, ADR, ADDADR, BIGENDIAN;

FROM ExStorage IMPORT
    ALLOCATE, DeallocateEx, HeapInfoPointer, GetHeap;

TYPE
  PBoxArray     = ARRAY [0..17] OF CARDINAL32;
  SBoxArray     = ARRAY [0..3],[0..255] OF CARDINAL32;

    BlowfishRec =
        RECORD
        sboxes          : SBoxArray;
        pbox            : PBoxArray;
        iv              : IV;
        heap            : HeapInfoPointer;
        END;
    Blowfish            = POINTER TO BlowfishRec;

CONST
    ORIG_P = PBoxArray{
                       0243F6A88h, 085A308D3h, 013198A2Eh, 003707344h,
                       0A4093822h, 0299F31D0h, 0082EFA98h, 0EC4E6C89h,
                       0452821E6h, 038D01377h, 0BE5466CFh, 034E90C6Ch,
                       0C0AC29B7h, 0C97C50DDh, 03F84D5B5h, 0B5470917h,
                       09216D5D9h, 08979FB1Bh
                       };

    ORIG_S = SBoxArray{

    {   0D1310BA6h, 098DFB5ACh, 02FFD72DBh, 0D01ADFB7h,
        0B8E1AFEDh, 06A267E96h, 0BA7C9045h, 0F12C7F99h,
        024A19947h, 0B3916CF7h, 00801F2E2h, 0858EFC16h,
        0636920D8h, 071574E69h, 0A458FEA3h, 0F4933D7Eh,
        00D95748Fh, 0728EB658h, 0718BCD58h, 082154AEEh,
        07B54A41Dh, 0C25A59B5h, 09C30D539h, 02AF26013h,
        0C5D1B023h, 0286085F0h, 0CA417918h, 0B8DB38EFh,
        08E79DCB0h, 0603A180Eh, 06C9E0E8Bh, 0B01E8A3Eh,
        0D71577C1h, 0BD314B27h, 078AF2FDAh, 055605C60h,
        0E65525F3h, 0AA55AB94h, 057489862h, 063E81440h,
        055CA396Ah, 02AAB10B6h, 0B4CC5C34h, 01141E8CEh,
        0A15486AFh, 07C72E993h, 0B3EE1411h, 0636FBC2Ah,
        02BA9C55Dh, 0741831F6h, 0CE5C3E16h, 09B87931Eh,
        0AFD6BA33h, 06C24CF5Ch, 07A325381h, 028958677h,
        03B8F4898h, 06B4BB9AFh, 0C4BFE81Bh, 066282193h,
        061D809CCh, 0FB21A991h, 0487CAC60h, 05DEC8032h,
        0EF845D5Dh, 0E98575B1h, 0DC262302h, 0EB651B88h,
        023893E81h, 0D396ACC5h, 00F6D6FF3h, 083F44239h,
        02E0B4482h, 0A4842004h, 069C8F04Ah, 09E1F9B5Eh,
        021C66842h, 0F6E96C9Ah, 0670C9C61h, 0ABD388F0h,
        06A51A0D2h, 0D8542F68h, 0960FA728h, 0AB5133A3h,
        06EEF0B6Ch, 0137A3BE4h, 0BA3BF050h, 07EFB2A98h,
        0A1F1651Dh, 039AF0176h, 066CA593Eh, 082430E88h,
        08CEE8619h, 0456F9FB4h, 07D84A5C3h, 03B8B5EBEh,
        0E06F75D8h, 085C12073h, 0401A449Fh, 056C16AA6h,
        04ED3AA62h, 0363F7706h, 01BFEDF72h, 0429B023Dh,
        037D0D724h, 0D00A1248h, 0DB0FEAD3h, 049F1C09Bh,
        0075372C9h, 080991B7Bh, 025D479D8h, 0F6E8DEF7h,
        0E3FE501Ah, 0B6794C3Bh, 0976CE0BDh, 004C006BAh,
        0C1A94FB6h, 0409F60C4h, 05E5C9EC2h, 0196A2463h,
        068FB6FAFh, 03E6C53B5h, 01339B2EBh, 03B52EC6Fh,
        06DFC511Fh, 09B30952Ch, 0CC814544h, 0AF5EBD09h,
        0BEE3D004h, 0DE334AFDh, 0660F2807h, 0192E4BB3h,
        0C0CBA857h, 045C8740Fh, 0D20B5F39h, 0B9D3FBDBh,
        05579C0BDh, 01A60320Ah, 0D6A100C6h, 0402C7279h,
        0679F25FEh, 0FB1FA3CCh, 08EA5E9F8h, 0DB3222F8h,
        03C7516DFh, 0FD616B15h, 02F501EC8h, 0AD0552ABh,
        0323DB5FAh, 0FD238760h, 053317B48h, 03E00DF82h,
        09E5C57BBh, 0CA6F8CA0h, 01A87562Eh, 0DF1769DBh,
        0D542A8F6h, 0287EFFC3h, 0AC6732C6h, 08C4F5573h,
        0695B27B0h, 0BBCA58C8h, 0E1FFA35Dh, 0B8F011A0h,
        010FA3D98h, 0FD2183B8h, 04AFCB56Ch, 02DD1D35Bh,
        09A53E479h, 0B6F84565h, 0D28E49BCh, 04BFB9790h,
        0E1DDF2DAh, 0A4CB7E33h, 062FB1341h, 0CEE4C6E8h,
        0EF20CADAh, 036774C01h, 0D07E9EFEh, 02BF11FB4h,
        095DBDA4Dh, 0AE909198h, 0EAAD8E71h, 06B93D5A0h,
        0D08ED1D0h, 0AFC725E0h, 08E3C5B2Fh, 08E7594B7h,
        08FF6E2FBh, 0F2122B64h, 08888B812h, 0900DF01Ch,
        04FAD5EA0h, 0688FC31Ch, 0D1CFF191h, 0B3A8C1ADh,
        02F2F2218h, 0BE0E1777h, 0EA752DFEh, 08B021FA1h,
        0E5A0CC0Fh, 0B56F74E8h, 018ACF3D6h, 0CE89E299h,
        0B4A84FE0h, 0FD13E0B7h, 07CC43B81h, 0D2ADA8D9h,
        0165FA266h, 080957705h, 093CC7314h, 0211A1477h,
        0E6AD2065h, 077B5FA86h, 0C75442F5h, 0FB9D35CFh,
        0EBCDAF0Ch, 07B3E89A0h, 0D6411BD3h, 0AE1E7E49h,
        000250E2Dh, 02071B35Eh, 0226800BBh, 057B8E0AFh,
        02464369Bh, 0F009B91Eh, 05563911Dh, 059DFA6AAh,
        078C14389h, 0D95A537Fh, 0207D5BA2h, 002E5B9C5h,
        083260376h, 06295CFA9h, 011C81968h, 04E734A41h,
        0B3472DCAh, 07B14A94Ah, 01B510052h, 09A532915h,
        0D60F573Fh, 0BC9BC6E4h, 02B60A476h, 081E67400h,
        008BA6FB5h, 0571BE91Fh, 0F296EC6Bh, 02A0DD915h,
        0B6636521h, 0E7B9F9B6h, 0FF34052Eh, 0C5855664h,
        053B02D5Dh, 0A99F8FA1h, 008BA4799h, 06E85076Ah   },

    {   04B7A70E9h, 0B5B32944h, 0DB75092Eh, 0C4192623h,
        0AD6EA6B0h, 049A7DF7Dh, 09CEE60B8h, 08FEDB266h,
        0ECAA8C71h, 0699A17FFh, 05664526Ch, 0C2B19EE1h,
        0193602A5h, 075094C29h, 0A0591340h, 0E4183A3Eh,
        03F54989Ah, 05B429D65h, 06B8FE4D6h, 099F73FD6h,
        0A1D29C07h, 0EFE830F5h, 04D2D38E6h, 0F0255DC1h,
        04CDD2086h, 08470EB26h, 06382E9C6h, 0021ECC5Eh,
        009686B3Fh, 03EBAEFC9h, 03C971814h, 06B6A70A1h,
        0687F3584h, 052A0E286h, 0B79C5305h, 0AA500737h,
        03E07841Ch, 07FDEAE5Ch, 08E7D44ECh, 05716F2B8h,
        0B03ADA37h, 0F0500C0Dh, 0F01C1F04h, 00200B3FFh,
        0AE0CF51Ah, 03CB574B2h, 025837A58h, 0DC0921BDh,
        0D19113F9h, 07CA92FF6h, 094324773h, 022F54701h,
        03AE5E581h, 037C2DADCh, 0C8B57634h, 09AF3DDA7h,
        0A9446146h, 00FD0030Eh, 0ECC8C73Eh, 0A4751E41h,
        0E238CD99h, 03BEA0E2Fh, 03280BBA1h, 0183EB331h,
        04E548B38h, 04F6DB908h, 06F420D03h, 0F60A04BFh,
        02CB81290h, 024977C79h, 05679B072h, 0BCAF89AFh,
        0DE9A771Fh, 0D9930810h, 0B38BAE12h, 0DCCF3F2Eh,
        05512721Fh, 02E6B7124h, 0501ADDE6h, 09F84CD87h,
        07A584718h, 07408DA17h, 0BC9F9ABCh, 0E94B7D8Ch,
        0EC7AEC3Ah, 0DB851DFAh, 063094366h, 0C464C3D2h,
        0EF1C1847h, 03215D908h, 0DD433B37h, 024C2BA16h,
        012A14D43h, 02A65C451h, 050940002h, 0133AE4DDh,
        071DFF89Eh, 010314E55h, 081AC77D6h, 05F11199Bh,
        0043556F1h, 0D7A3C76Bh, 03C11183Bh, 05924A509h,
        0F28FE6EDh, 097F1FBFAh, 09EBABF2Ch, 01E153C6Eh,
        086E34570h, 0EAE96FB1h, 0860E5E0Ah, 05A3E2AB3h,
        0771FE71Ch, 04E3D06FAh, 02965DCB9h, 099E71D0Fh,
        0803E89D6h, 05266C825h, 02E4CC978h, 09C10B36Ah,
        0C6150EBAh, 094E2EA78h, 0A5FC3C53h, 01E0A2DF4h,
        0F2F74EA7h, 0361D2B3Dh, 01939260Fh, 019C27960h,
        05223A708h, 0F71312B6h, 0EBADFE6Eh, 0EAC31F66h,
        0E3BC4595h, 0A67BC883h, 0B17F37D1h, 0018CFF28h,
        0C332DDEFh, 0BE6C5AA5h, 065582185h, 068AB9802h,
        0EECEA50Fh, 0DB2F953Bh, 02AEF7DADh, 05B6E2F84h,
        01521B628h, 029076170h, 0ECDD4775h, 0619F1510h,
        013CCA830h, 0EB61BD96h, 00334FE1Eh, 0AA0363CFh,
        0B5735C90h, 04C70A239h, 0D59E9E0Bh, 0CBAADE14h,
        0EECC86BCh, 060622CA7h, 09CAB5CABh, 0B2F3846Eh,
        0648B1EAFh, 019BDF0CAh, 0A02369B9h, 0655ABB50h,
        040685A32h, 03C2AB4B3h, 0319EE9D5h, 0C021B8F7h,
        09B540B19h, 0875FA099h, 095F7997Eh, 0623D7DA8h,
        0F837889Ah, 097E32D77h, 011ED935Fh, 016681281h,
        00E358829h, 0C7E61FD6h, 096DEDFA1h, 07858BA99h,
        057F584A5h, 01B227263h, 09B83C3FFh, 01AC24696h,
        0CDB30AEBh, 0532E3054h, 08FD948E4h, 06DBC3128h,
        058EBF2EFh, 034C6FFEAh, 0FE28ED61h, 0EE7C3C73h,
        05D4A14D9h, 0E864B7E3h, 042105D14h, 0203E13E0h,
        045EEE2B6h, 0A3AAABEAh, 0DB6C4F15h, 0FACB4FD0h,
        0C742F442h, 0EF6ABBB5h, 0654F3B1Dh, 041CD2105h,
        0D81E799Eh, 086854DC7h, 0E44B476Ah, 03D816250h,
        0CF62A1F2h, 05B8D2646h, 0FC8883A0h, 0C1C7B6A3h,
        07F1524C3h, 069CB7492h, 047848A0Bh, 05692B285h,
        0095BBF00h, 0AD19489Dh, 01462B174h, 023820E00h,
        058428D2Ah, 00C55F5EAh, 01DADF43Eh, 0233F7061h,
        03372F092h, 08D937E41h, 0D65FECF1h, 06C223BDBh,
        07CDE3759h, 0CBEE7460h, 04085F2A7h, 0CE77326Eh,
        0A6078084h, 019F8509Eh, 0E8EFD855h, 061D99735h,
        0A969A7AAh, 0C50C06C2h, 05A04ABFCh, 0800BCADCh,
        09E447A2Eh, 0C3453484h, 0FDD56705h, 00E1E9EC9h,
        0DB73DBD3h, 0105588CDh, 0675FDA79h, 0E3674340h,
        0C5C43465h, 0713E38D8h, 03D28F89Eh, 0F16DFF20h,
        0153E21E7h, 08FB03D4Ah, 0E6E39F2Bh, 0DB83ADF7h   },

    {   0E93D5A68h, 0948140F7h, 0F64C261Ch, 094692934h,
        0411520F7h, 07602D4F7h, 0BCF46B2Eh, 0D4A20068h,
        0D4082471h, 03320F46Ah, 043B7D4B7h, 0500061AFh,
        01E39F62Eh, 097244546h, 014214F74h, 0BF8B8840h,
        04D95FC1Dh, 096B591AFh, 070F4DDD3h, 066A02F45h,
        0BFBC09ECh, 003BD9785h, 07FAC6DD0h, 031CB8504h,
        096EB27B3h, 055FD3941h, 0DA2547E6h, 0ABCA0A9Ah,
        028507825h, 0530429F4h, 00A2C86DAh, 0E9B66DFBh,
        068DC1462h, 0D7486900h, 0680EC0A4h, 027A18DEEh,
        04F3FFEA2h, 0E887AD8Ch, 0B58CE006h, 07AF4D6B6h,
        0AACE1E7Ch, 0D3375FECh, 0CE78A399h, 0406B2A42h,
        020FE9E35h, 0D9F385B9h, 0EE39D7ABh, 03B124E8Bh,
        01DC9FAF7h, 04B6D1856h, 026A36631h, 0EAE397B2h,
        03A6EFA74h, 0DD5B4332h, 06841E7F7h, 0CA7820FBh,
        0FB0AF54Eh, 0D8FEB397h, 0454056ACh, 0BA489527h,
        055533A3Ah, 020838D87h, 0FE6BA9B7h, 0D096954Bh,
        055A867BCh, 0A1159A58h, 0CCA92963h, 099E1DB33h,
        0A62A4A56h, 03F3125F9h, 05EF47E1Ch, 09029317Ch,
        0FDF8E802h, 004272F70h, 080BB155Ch, 005282CE3h,
        095C11548h, 0E4C66D22h, 048C1133Fh, 0C70F86DCh,
        007F9C9EEh, 041041F0Fh, 0404779A4h, 05D886E17h,
        0325F51EBh, 0D59BC0D1h, 0F2BCC18Fh, 041113564h,
        0257B7834h, 0602A9C60h, 0DFF8E8A3h, 01F636C1Bh,
        00E12B4C2h, 002E1329Eh, 0AF664FD1h, 0CAD18115h,
        06B2395E0h, 0333E92E1h, 03B240B62h, 0EEBEB922h,
        085B2A20Eh, 0E6BA0D99h, 0DE720C8Ch, 02DA2F728h,
        0D0127845h, 095B794FDh, 0647D0862h, 0E7CCF5F0h,
        05449A36Fh, 0877D48FAh, 0C39DFD27h, 0F33E8D1Eh,
        00A476341h, 0992EFF74h, 03A6F6EABh, 0F4F8FD37h,
        0A812DC60h, 0A1EBDDF8h, 0991BE14Ch, 0DB6E6B0Dh,
        0C67B5510h, 06D672C37h, 02765D43Bh, 0DCD0E804h,
        0F1290DC7h, 0CC00FFA3h, 0B5390F92h, 0690FED0Bh,
        0667B9FFBh, 0CEDB7D9Ch, 0A091CF0Bh, 0D9155EA3h,
        0BB132F88h, 0515BAD24h, 07B9479BFh, 0763BD6EBh,
        037392EB3h, 0CC115979h, 08026E297h, 0F42E312Dh,
        06842ADA7h, 0C66A2B3Bh, 012754CCCh, 0782EF11Ch,
        06A124237h, 0B79251E7h, 006A1BBE6h, 04BFB6350h,
        01A6B1018h, 011CAEDFAh, 03D25BDD8h, 0E2E1C3C9h,
        044421659h, 00A121386h, 0D90CEC6Eh, 0D5ABEA2Ah,
        064AF674Eh, 0DA86A85Fh, 0BEBFE988h, 064E4C3FEh,
        09DBC8057h, 0F0F7C086h, 060787BF8h, 06003604Dh,
        0D1FD8346h, 0F6381FB0h, 07745AE04h, 0D736FCCCh,
        083426B33h, 0F01EAB71h, 0B0804187h, 03C005E5Fh,
        077A057BEh, 0BDE8AE24h, 055464299h, 0BF582E61h,
        04E58F48Fh, 0F2DDFDA2h, 0F474EF38h, 08789BDC2h,
        05366F9C3h, 0C8B38E74h, 0B475F255h, 046FCD9B9h,
        07AEB2661h, 08B1DDF84h, 0846A0E79h, 0915F95E2h,
        0466E598Eh, 020B45770h, 08CD55591h, 0C902DE4Ch,
        0B90BACE1h, 0BB8205D0h, 011A86248h, 07574A99Eh,
        0B77F19B6h, 0E0A9DC09h, 0662D09A1h, 0C4324633h,
        0E85A1F02h, 009F0BE8Ch, 04A99A025h, 01D6EFE10h,
        01AB93D1Dh, 00BA5A4DFh, 0A186F20Fh, 02868F169h,
        0DCB7DA83h, 0573906FEh, 0A1E2CE9Bh, 04FCD7F52h,
        050115E01h, 0A70683FAh, 0A002B5C4h, 00DE6D027h,
        09AF88C27h, 0773F8641h, 0C3604C06h, 061A806B5h,
        0F0177A28h, 0C0F586E0h, 0006058AAh, 030DC7D62h,
        011E69ED7h, 02338EA63h, 053C2DD94h, 0C2C21634h,
        0BBCBEE56h, 090BCB6DEh, 0EBFC7DA1h, 0CE591D76h,
        06F05E409h, 04B7C0188h, 039720A3Dh, 07C927C24h,
        086E3725Fh, 0724D9DB9h, 01AC15BB4h, 0D39EB8FCh,
        0ED545578h, 008FCA5B5h, 0D83D7CD3h, 04DAD0FC4h,
        01E50EF5Eh, 0B161E6F8h, 0A28514D9h, 06C51133Ch,
        06FD5C7E7h, 056E14EC4h, 0362ABFCEh, 0DDC6C837h,
        0D79A3234h, 092638212h, 0670EFA8Eh, 0406000E0h  },

    {   03A39CE37h, 0D3FAF5CFh, 0ABC27737h, 05AC52D1Bh,
        05CB0679Eh, 04FA33742h, 0D3822740h, 099BC9BBEh,
        0D5118E9Dh, 0BF0F7315h, 0D62D1C7Eh, 0C700C47Bh,
        0B78C1B6Bh, 021A19045h, 0B26EB1BEh, 06A366EB4h,
        05748AB2Fh, 0BC946E79h, 0C6A376D2h, 06549C2C8h,
        0530FF8EEh, 0468DDE7Dh, 0D5730A1Dh, 04CD04DC6h,
        02939BBDBh, 0A9BA4650h, 0AC9526E8h, 0BE5EE304h,
        0A1FAD5F0h, 06A2D519Ah, 063EF8CE2h, 09A86EE22h,
        0C089C2B8h, 043242EF6h, 0A51E03AAh, 09CF2D0A4h,
        083C061BAh, 09BE96A4Dh, 08FE51550h, 0BA645BD6h,
        02826A2F9h, 0A73A3AE1h, 04BA99586h, 0EF5562E9h,
        0C72FEFD3h, 0F752F7DAh, 03F046F69h, 077FA0A59h,
        080E4A915h, 087B08601h, 09B09E6ADh, 03B3EE593h,
        0E990FD5Ah, 09E34D797h, 02CF0B7D9h, 0022B8B51h,
        096D5AC3Ah, 0017DA67Dh, 0D1CF3ED6h, 07C7D2D28h,
        01F9F25CFh, 0ADF2B89Bh, 05AD6B472h, 05A88F54Ch,
        0E029AC71h, 0E019A5E6h, 047B0ACFDh, 0ED93FA9Bh,
        0E8D3C48Dh, 0283B57CCh, 0F8D56629h, 079132E28h,
        0785F0191h, 0ED756055h, 0F7960E44h, 0E3D35E8Ch,
        015056DD4h, 088F46DBAh, 003A16125h, 00564F0BDh,
        0C3EB9E15h, 03C9057A2h, 097271AECh, 0A93A072Ah,
        01B3F6D9Bh, 01E6321F5h, 0F59C66FBh, 026DCF319h,
        07533D928h, 0B155FDF5h, 003563482h, 08ABA3CBBh,
        028517711h, 0C20AD9F8h, 0ABCC5167h, 0CCAD925Fh,
        04DE81751h, 03830DC8Eh, 0379D5862h, 09320F991h,
        0EA7A90C2h, 0FB3E7BCEh, 05121CE64h, 0774FBE32h,
        0A8B6E37Eh, 0C3293D46h, 048DE5369h, 06413E680h,
        0A2AE0810h, 0DD6DB224h, 069852DFDh, 009072166h,
        0B39A460Ah, 06445C0DDh, 0586CDECFh, 01C20C8AEh,
        05BBEF7DDh, 01B588D40h, 0CCD2017Fh, 06BB4E3BBh,
        0DDA26A7Eh, 03A59FF45h, 03E350A44h, 0BCB4CDD5h,
        072EACEA8h, 0FA6484BBh, 08D6612AEh, 0BF3C6F47h,
        0D29BE463h, 0542F5D9Eh, 0AEC2771Bh, 0F64E6370h,
        0740E0D8Dh, 0E75B1357h, 0F8721671h, 0AF537D5Dh,
        04040CB08h, 04EB4E2CCh, 034D2466Ah, 00115AF84h,
        0E1B00428h, 095983A1Dh, 006B89FB4h, 0CE6EA048h,
        06F3F3B82h, 03520AB82h, 0011A1D4Bh, 0277227F8h,
        0611560B1h, 0E7933FDCh, 0BB3A792Bh, 0344525BDh,
        0A08839E1h, 051CE794Bh, 02F32C9B7h, 0A01FBAC9h,
        0E01CC87Eh, 0BCC7D1F6h, 0CF0111C3h, 0A1E8AAC7h,
        01A908749h, 0D44FBD9Ah, 0D0DADECBh, 0D50ADA38h,
        00339C32Ah, 0C6913667h, 08DF9317Ch, 0E0B12B4Fh,
        0F79E59B7h, 043F5BB3Ah, 0F2D519FFh, 027D9459Ch,
        0BF97222Ch, 015E6FC2Ah, 00F91FC71h, 09B941525h,
        0FAE59361h, 0CEB69CEBh, 0C2A86459h, 012BAA8D1h,
        0B6C1075Eh, 0E3056A0Ch, 010D25065h, 0CB03A442h,
        0E0EC6E0Eh, 01698DB3Bh, 04C98A0BEh, 03278E964h,
        09F1F9532h, 0E0D392DFh, 0D3A0342Bh, 08971F21Eh,
        01B0A7441h, 04BA3348Ch, 0C5BE7120h, 0C37632D8h,
        0DF359F8Dh, 09B992F2Eh, 0E60B6F47h, 00FE3F11Dh,
        0E54CDA54h, 01EDAD891h, 0CE6279CFh, 0CD3E7E6Fh,
        01618B166h, 0FD2C1D05h, 0848FD2C5h, 0F6FB2299h,
        0F523F357h, 0A6327623h, 093A83531h, 056CCCD02h,
        0ACF08162h, 05A75EBB5h, 06E163697h, 088D273CCh,
        0DE966292h, 081B949D0h, 04C50901Bh, 071C65614h,
        0E6C6C7BDh, 0327A140Ah, 045E1D006h, 0C3F27B9Ah,
        0C9AA53FDh, 062A80F00h, 0BB25BFE2h, 035BDD2F6h,
        071126905h, 0B2040222h, 0B6CBCF7Ch, 0CD769C2Bh,
        053113EC0h, 01640E3D3h, 038ABBD60h, 02547ADF0h,
        0BA38209Ch, 0F746CE76h, 077AFA1C5h, 020756060h,
        085CBFE4Eh, 08AE88DD8h, 07AAAF9B0h, 04CF9AA7Eh,
        01948C25Ch, 002FB8A8Ch, 001C36AE4h, 0D6EBE1F9h,
        090D4F869h, 0A65CDEA0h, 03F09252Dh, 0C208E69Fh,
        0B74E6132h, 0CE77E25Bh, 0578FDFE3h, 03AC372E6h  }
    };

PROCEDURE F(crypt : Blowfish; x : CARDINAL32) : CARDINAL32 [INLINE];
VAR
    y   : CARDINAL32;
BEGIN
    (* faster to do the shifts than access bytes via an array subscript.
       significantly faster.
       faster because it eliminates memory writes in the core loop.
       rememember that arrays must exist in memory.
    *)
    y := crypt^.sboxes[0][(x SHR 24) BAND 0FFh] +
                crypt^.sboxes[1][(x SHR 16) BAND 0FFh];

    y := y BXOR crypt^.sboxes[2][(x SHR 8) BAND 0FFh];

    y := y + crypt^.sboxes[3][x BAND 0FFh];

    RETURN y;
END F;

PROCEDURE ROUND(crypt : Blowfish;
                round : CARDINAL;
                VAR INOUT left, right : CARDINAL32) [INLINE];
BEGIN
    left := left BXOR crypt^.pbox[round];
    right := right BXOR F(crypt, left);
END ROUND;

PROCEDURE encrypt(crypt : Blowfish; VAR INOUT left, right : CARDINAL32);
VAR
    ll, rr      : CARDINAL32;
BEGIN
    ll := left;
    rr := right;

    ROUND(crypt, 0, ll, rr);
    ROUND(crypt, 1, rr, ll);
    ROUND(crypt, 2, ll, rr);
    ROUND(crypt, 3, rr, ll);
    ROUND(crypt, 4, ll, rr);
    ROUND(crypt, 5, rr, ll);
    ROUND(crypt, 6, ll, rr);
    ROUND(crypt, 7, rr, ll);
    ROUND(crypt, 8, ll, rr);
    ROUND(crypt, 9, rr, ll);
    ROUND(crypt, 10, ll, rr);
    ROUND(crypt, 11, rr, ll);
    ROUND(crypt, 12, ll, rr);
    ROUND(crypt, 13, rr, ll);
    ROUND(crypt, 14, ll, rr);
    ROUND(crypt, 15, rr, ll);

    right := ll BXOR crypt^.pbox[16];
    left := rr BXOR crypt^.pbox[17];
END encrypt;

PROCEDURE decrypt(crypt : Blowfish; VAR INOUT left, right : CARDINAL32);
VAR
    ll, rr      : CARDINAL32;
BEGIN
    ll := left;
    rr := right;

    ROUND(crypt, 17, ll, rr);
    ROUND(crypt, 16, rr, ll);
    ROUND(crypt, 15, ll, rr);
    ROUND(crypt, 14, rr, ll);
    ROUND(crypt, 13, ll, rr);
    ROUND(crypt, 12, rr, ll);
    ROUND(crypt, 11, ll, rr);
    ROUND(crypt, 10, rr, ll);
    ROUND(crypt, 9, ll, rr);
    ROUND(crypt, 8, rr, ll);
    ROUND(crypt, 7, ll, rr);
    ROUND(crypt, 6, rr, ll);
    ROUND(crypt, 5, ll, rr);
    ROUND(crypt, 4, rr, ll);
    ROUND(crypt, 3, ll, rr);
    ROUND(crypt, 2, rr, ll);

    right := ll BXOR crypt^.pbox[1];
    left := rr BXOR crypt^.pbox[0];
END decrypt;

PROCEDURE KeySetup(crypt : Blowfish; key : ARRAY OF BYTE; keySize : CARDINAL);
VAR
    i, j, k     : CARDINAL;
    data        : CARDINAL32;
    datal,
    datar       : CARDINAL32;
BEGIN
    crypt^.sboxes := ORIG_S;

    k := 0;
    FOR i := 0 TO 17 DO
        data := ORD(key[k]);
        k := (k+1) REM keySize;
        data := (data SHL 8) BOR ORD(key[k]);
        k := (k+1) REM keySize;
        data := (data SHL 8) BOR ORD(key[k]);
        k := (k+1) REM keySize;
        data := (data SHL 8) BOR ORD(key[k]);
        k := (k+1) REM keySize;

        crypt^.pbox[i] := ORIG_P[i] BXOR data;
    END;

    datal := 0;
    datar := 0;
    FOR i := 0 TO 16 BY 2 DO
        encrypt(crypt, datal, datar);
        crypt^.pbox[i] := datal;
        crypt^.pbox[i+1] := datar;
    END;
    FOR i := 0 TO 3 DO
        FOR j := 0 TO 254 BY 2 DO
            encrypt(crypt, datal, datar);
            crypt^.sboxes[i, j] := datal;
            crypt^.sboxes[i, j+1] := datar;
        END;
    END;
END KeySetup;

PROCEDURE Create(key : ARRAY OF BYTE; keySize : CARDINAL) : Blowfish;
VAR
    crypt       : Blowfish;
BEGIN
    NEW(crypt);
    crypt^.heap := GetHeap();

    KeySetup(crypt, key, keySize);

    RETURN crypt;
END Create;

PROCEDURE Create2(key : ARRAY OF CHAR) : Blowfish;
VAR
    l   : CARDINAL;
BEGIN
    l := LENGTH(key);
    IF l = 0 THEN
        l := 1;
    END;
    RETURN Create(key, l*SIZE(CHAR));
END Create2;

PROCEDURE Destroy(VAR INOUT crypt : Blowfish);
BEGIN
    DeallocateEx(crypt, SIZE(crypt^), crypt^.heap);
END Destroy;

PROCEDURE ResetIV(crypt : Blowfish; iv : IV);
BEGIN
    crypt^.iv := iv;
END ResetIV;

PROCEDURE load(input : ADDRESS; VAR OUT low, high : CARDINAL32) [INLINE];
VAR
    data        : POINTER TO ARRAY [0..1] OF CARDINAL32;
BEGIN
    (* alignment and endian dependent but faster.
       2 reads as opposed to 8.
    *)
    data := input;
    low := BIGENDIAN(data^[0]);
    high := BIGENDIAN(data^[1]);
END load;

PROCEDURE store(low, high : CARDINAL32; output : ADDRESS) [INLINE];
VAR
    data        : POINTER TO ARRAY [0..1] OF CARDINAL32;
BEGIN
    (* alignment and endian dependent but faster.
       2 writes as opposed to 8.
    *)
    data := output;
    data^[0] := BIGENDIAN(low);
    data^[1] := BIGENDIAN(high);
END store;

PROCEDURE EncryptECB(crypt : Blowfish; input, output : ADDRESS; amount : CARDINAL);
VAR
    left, right : CARDINAL32;
BEGIN
    IF (amount REM BlockSize) = 0 THEN
        REPEAT
            amount := amount - BlockSize;

            load(input, left, right);
            encrypt(crypt, left, right);
            store(left, right, output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);
        UNTIL amount = 0;
    END;
END EncryptECB;

PROCEDURE DecryptECB(crypt : Blowfish; input, output : ADDRESS; amount : CARDINAL);
VAR
    left, right : CARDINAL32;
BEGIN
    IF (amount REM BlockSize) = 0 THEN
        REPEAT
            amount := amount - BlockSize;

            load(input, left, right);
            decrypt(crypt, left, right);
            store(left, right, output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);
        UNTIL amount = 0;
    END;
END DecryptECB;

PROCEDURE EncryptCBC(crypt : Blowfish; input, output : ADDRESS; amount : CARDINAL);
VAR
    ivl, ivr    : CARDINAL32;
    left, right : CARDINAL32;
BEGIN
    IF (amount REM BlockSize) = 0 THEN
        load(ADR(crypt^.iv.bytes), ivl, ivr);

        REPEAT
            amount := amount - BlockSize;

            load(input, left, right);
            left := left BXOR ivl;
            right := right BXOR ivr;
            encrypt(crypt, left, right);
            store(left, right, output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);

            ivl := left;
            ivr := right;
        UNTIL amount = 0;

        store(ivl, ivr, ADR(crypt^.iv.bytes));
    END;
END EncryptCBC;

PROCEDURE DecryptCBC(crypt : Blowfish; input, output : ADDRESS; amount : CARDINAL);
VAR
    ivl, ivr            : CARDINAL32;
    saveL, saveR        : CARDINAL32;
    left, right         : CARDINAL32;
BEGIN
    IF (amount REM BlockSize) = 0 THEN
        load(ADR(crypt^.iv.bytes), ivl, ivr);

        REPEAT
            amount := amount - BlockSize;

            load(input, left, right);
            saveL := left;
            saveR := right;
            decrypt(crypt, left, right);
            store(left BXOR ivl, right BXOR ivr, output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);

            ivl := saveL;
            ivr := saveR;
        UNTIL amount = 0;

        store(ivl, ivr, ADR(crypt^.iv.bytes));
    END;
END DecryptCBC;

PROCEDURE EncryptCFB(crypt : Blowfish; input, output : ADDRESS; amount : CARDINAL);
VAR
    i           : CARDINAL;
    inl, inr    : CARDINAL32;
    left, right : CARDINAL32;
    result      : ARRAY [0..BlockSize-1] OF BYTE;
    inP, outP   : POINTER TO ARRAY [0..BlockSize-1] OF BYTE;
BEGIN
    load(ADR(crypt^.iv.bytes), left, right);

    REPEAT
        encrypt(crypt, left, right);

        IF amount >= BlockSize THEN
            (* full block *)
            amount := amount - BlockSize;

            load(input, inl, inr);
            left := left BXOR inl;
            right := right BXOR inr;
            store(left, right, output);

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);
        ELSE
            (* partial block *)
            store(left, right, ADR(result));
            inP := input;
            outP := output;
            FOR i := 0 TO amount-1 DO
                outP^[i] := inP^[i] BXOR result[i];
            END;
            amount := 0;
        END;
    UNTIL amount = 0;

    store(left, right, ADR(crypt^.iv.bytes));
END EncryptCFB;

PROCEDURE DecryptCFB(crypt : Blowfish; input, output : ADDRESS; amount : CARDINAL);
VAR
    i                   : CARDINAL;
    inl, inr            : CARDINAL32;
    left, right         : CARDINAL32;
    result              : ARRAY [0..BlockSize-1] OF BYTE;
    inP, outP           : POINTER TO ARRAY [0..BlockSize-1] OF BYTE;
BEGIN
    load(ADR(crypt^.iv.bytes), left, right);

    REPEAT
        encrypt(crypt, left, right);

        IF amount >= BlockSize THEN
            (* full block *)
            amount := amount - BlockSize;

            load(input, inl, inr);
            store(left BXOR inl, right BXOR inr, output);
            left := inl;
            right := inr;

            input := ADDADR(input, BlockSize);
            output := ADDADR(output, BlockSize);
        ELSE
            (* partial block *)
            store(left, right, ADR(result));
            inP := input;
            outP := output;
            FOR i := 0 TO amount-1 DO
                outP^[i] := inP^[i] BXOR result[i];
            END;
            amount := 0;
        END;

    UNTIL amount = 0;

    store(left, right, ADR(crypt^.iv.bytes));
END DecryptCFB;

PROCEDURE SelfTest() : BOOLEAN;
(* test vectors obtained from www.counterpane.com, the authors of Blowfish *)
TYPE
    tArray      = ARRAY [0..BlockSize-1] OF CARDINAL8;

    tAlign = (* force 4-byte alignment of our test buffers *)
    RECORD
    dummy       : CARDINAL32;
    input,
    result      : tArray;
    input1      : ARRAY [0..31] OF BYTE;
    result1     : ARRAY [0..31] OF BYTE;
    END;

CONST
    key         : ARRAY [0..2] OF tArray =
    {
     {11h BY 8},
     {01h, 23h, 45h, 67h, 89h, 0abh, 0cdh, 0efh},
     {0feh, 0dch, 0bah, 98h, 76h, 54h, 32h, 10h}
    };

    input       : ARRAY [0..2] OF tArray =
    {
     {11h BY 8},
     {11h BY 8},
     {01h, 23h, 45h, 67h, 89h, 0abh, 0cdh, 0efh}
    };

    output      : ARRAY [0..2] OF tArray =
    {
     {24h, 66h, 0DDh, 87h, 8Bh, 96h, 3Ch, 9Dh},
     {61h, 0F9h, 0C3h, 80h, 22h, 81h, 0B0h, 96h},
     {0Ah, 0CEh, 0ABh, 0Fh, 0C6h, 0A0h, 0A2h, 8Dh}
    };

    chainKey : ARRAY [0..15] OF CARDINAL8 =
    {
     01h, 23h, 45h, 67h, 89h, 0abh, 0cdh, 0efh,
     0F0h, 0E1h, 0D2h, 0C3h, 0B4h, 0A5h, 96h, 87h
    };

    chainInput : ARRAY [0..31] OF CARDINAL8 =
    {
     037h, 036h, 035h, 034h, 033h, 032h, 031h, 020h,
     04Eh, 06Fh, 077h, 020h, 069h, 073h, 020h, 074h,
     068h, 065h, 020h, 074h, 069h, 06Dh, 065h, 020h,
     066h, 06Fh, 072h, 020h, 000h, 000h, 000h, 000h
    };

    chainIV = tArray{0FEh, 0DCh, 0BAh, 098h, 076h, 054h, 032h, 010h};

    resultCBC : ARRAY [0..31] OF CARDINAL8 =
    {
     06Bh, 077h, 0B4h, 0D6h, 030h, 006h, 0DEh, 0E6h,
     005h, 0B1h, 056h, 0E2h, 074h, 003h, 097h, 093h,
     058h, 0DEh, 0B9h, 0E7h, 015h, 046h, 016h, 0D9h,
     059h, 0F1h, 065h, 02Bh, 0D5h, 0FFh, 092h, 0CCh
    };

    resultCFB : ARRAY [0..28] OF CARDINAL8 =
    {
     0E7h, 032h, 014h, 0A2h, 082h, 021h, 039h, 0CAh,
     0F2h, 06Eh, 0CFh, 06Dh, 02Eh, 0B9h, 0E7h, 06Eh,
     03Dh, 0A3h, 0DEh, 004h, 0D1h, 051h, 072h, 000h,
     051h, 09Dh, 057h, 0A6h, 0C3h
    };

VAR
    crypt       : Blowfish;
    data        : tAlign;
    iv          : IV;
    ok          : BOOLEAN;
    i           : CARDINAL;

    PROCEDURE verify(a, b : ARRAY OF BYTE) : BOOLEAN;
    VAR
        i       : CARDINAL;
    BEGIN
        FOR i := 0 TO HIGH(a) DO
            IF a[i] <> b[i] THEN
                RETURN FALSE;
            END;
        END;
        RETURN TRUE;
    END verify;

BEGIN
    ok := TRUE;

    crypt := Create(key[0], SIZE(key[0]));

    FOR i := 0 TO HIGH(key) DO
        KeySetup(crypt, key[i], SIZE(key[0]));

        data.input := input[i];
        EncryptECB(crypt, ADR(data.input), ADR(data.result), 8);
        ok := ok AND verify(data.result, output[i]);
        DecryptECB(crypt, ADR(data.result), ADR(data.result), 8);
        ok := ok AND verify(data.result, input[i]);
    END;

    (* now test the chaining modes *)

    KeySetup(crypt, chainKey, SIZE(chainKey));
    iv.bytes[0..7] := chainIV;
    data.input1[0..28] := chainInput;
    data.input1[29] := 0;
    data.input1[30] := 0;
    data.input1[31] := 0;

    (* test CBC *)

    ResetIV(crypt, iv);
    EncryptCBC(crypt, ADR(data.input1), ADR(data.result1), 16);
    EncryptCBC(crypt, ADR(data.input1[16]), ADR(data.result1[16]), 16);
    ok := ok AND verify(data.result1, resultCBC);
    ResetIV(crypt, iv);
    DecryptCBC(crypt, ADR(data.result1), ADR(data.result1), 16);
    DecryptCBC(crypt, ADR(data.result1[16]), ADR(data.result1[16]), 16);
    ok := ok AND verify(data.result1, data.input1);

    (* test CFB *)

    ResetIV(crypt, iv);
    EncryptCFB(crypt, ADR(data.input1), ADR(data.result1), 16);
    EncryptCFB(crypt, ADR(data.input1[16]), ADR(data.result1[16]), 13);
    ok := ok AND verify(data.result1[0..28], resultCFB);
    ResetIV(crypt, iv);
    DecryptCFB(crypt, ADR(data.result1), ADR(data.result1), 16);
    DecryptCFB(crypt, ADR(data.result1[16]), ADR(data.result1[16]), 13);
    ok := ok AND verify(data.result1[0..28], data.input1);

    Destroy(crypt);

    RETURN ok;
END SelfTest;

PROCEDURE IsWeak(crypt : Blowfish) : BOOLEAN;
VAR
    main        : CARDINAL;
    sub         : CARDINAL;
BEGIN
    main := 0;
    REPEAT
        sub := main+1;
        REPEAT
            IF (crypt^.sboxes[0, main] = crypt^.sboxes[0, sub]) OR
               (crypt^.sboxes[1, main] = crypt^.sboxes[1, sub]) OR
               (crypt^.sboxes[2, main] = crypt^.sboxes[2, sub]) OR
               (crypt^.sboxes[3, main] = crypt^.sboxes[3, sub])
            THEN
                RETURN TRUE;
            END;

            INC(sub);
        UNTIL sub = 256;

        INC(main);
    UNTIL main = 255;

    RETURN FALSE;
END IsWeak;

END Blowfish.
