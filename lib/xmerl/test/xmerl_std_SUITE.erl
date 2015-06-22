%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2011. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
%%----------------------------------------------------------------------
-module(xmerl_std_SUITE).

-compile(export_all).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("test_server/include/test_server.hrl").
%%-include("xmerl.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(ibm_dir,"ibm").
-define(sun_dir,"sun").
-define(japanese_dir,"japanese").
-define(oasis_dir,"oasis").
-define(xmltest_dir,"xmltest").

%%======================================================================
%% Tests
%%======================================================================

%%----------------------------------------------------------------------
%% Test groups
%%----------------------------------------------------------------------
%% totally 1788 test cases
all() -> 
    ['sun-valid'(suite), 'sun-invalid'(suite),
     'sun-not-wf'(suite), 'sun-error'(suite),
     'jclark-xmltest'(suite), 'xerox-japanese'(suite),
     'nist-oasis'(suite), 'ibm-invalid'(suite),
     'ibm-not-wf'(suite), 'ibm-valid'(suite)].

groups() -> 
    [{sun_test_cases, [],
      'sun-valid'(suite) ++
	  'sun-invalid'(suite) ++
	  'sun-not-wf'(suite) ++ 'sun-error'(suite)},
     {ibm_test_cases, [],
      'ibm-invalid'(suite) ++
	  'ibm-not-wf'(suite) ++ 'ibm-valid'(suite)},
     {xmltest_test_cases, [], 'jclark-xmltest'(suite)},
     {japanese_test_cases, [], 'xerox-japanese'(suite)},
     {oasis_test_cases, [], 'nist-oasis'(suite)}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



'sun-valid'(suite) -> %% 28 test cases
    ['v-pe02','v-pe03','v-pe00','v-lang06','v-lang05','v-lang04',
     'v-lang03','v-lang02','v-lang01','v-sgml01',sa05,sa04,sa03,sa02,
     sa01,required00,optional,notation01,'not-sa04','not-sa03',
     'not-sa02','not-sa01',ext02,ext01,element,dtd01,dtd00,pe01].
'sun-invalid'(suite) -> %% 74 test cases
    [empty,utf16l,utf16b,attr16,attr15,attr14,attr13,attr12,attr11,
     attr10,attr09,attr08,attr07,attr06,attr05,attr04,attr03,attr02,
     attr01,root,'inv-required02','inv-required01','inv-required00',
     optional25,optional24,optional23,optional22,optional21,optional20,
     optional14,optional13,optional12,optional11,optional10,optional09,
     optional08,optional07,optional06,optional05,optional04,optional03,
     optional02,optional01,'inv-not-sa14','inv-not-sa13','inv-not-sa12',
     'inv-not-sa11','inv-not-sa10','inv-not-sa09','inv-not-sa08',
     'inv-not-sa07','inv-not-sa06','inv-not-sa05','inv-not-sa04',
     'inv-not-sa02','inv-not-sa01',id09,id08,id07,id06,id05,id04,id03,
     id02,id01,el06,el05,el04,el03,el02,el01,'inv-dtd03','inv-dtd02',
     'inv-dtd01'].
'sun-not-wf'(suite)-> %% 56 test cases
    [sgml13,sgml12,sgml11,sgml10,sgml09,sgml08,sgml07,sgml06,sgml05,
     sgml04,sgml03,sgml02,sgml01,pubid05,pubid04,pubid03,pubid02,pubid01,
     pi,encoding07,encoding06,encoding05,encoding04,encoding03,encoding02,
     encoding01,element04,element03,element02,element01,element00,dtd07,
     dtd05,dtd04,dtd03,dtd02,'nwf-dtd01','nwf-dtd00',decl01,content03,
     content02,content01,cond02,cond01,attlist11,attlist10,attlist09,
     attlist08,attlist07,attlist06,attlist05,attlist04,attlist03,
     attlist02,attlist01,'not-wf-sa03'].
'sun-error'(suite)-> 
    %% 1 test case 
    [uri01].  
'jclark-xmltest'(suite) -> 
    %% 364 test cases
    ['valid-ext-sa-014','valid-ext-sa-013','valid-ext-sa-012',
     'valid-ext-sa-011','valid-ext-sa-009','valid-ext-sa-008',
     'valid-ext-sa-007','valid-ext-sa-006','valid-ext-sa-005',
     'valid-ext-sa-004','valid-ext-sa-003','valid-ext-sa-002',
     'valid-ext-sa-001','valid-not-sa-031','valid-not-sa-030',
     'valid-not-sa-029','valid-not-sa-028','valid-not-sa-027',
     'valid-not-sa-026','valid-not-sa-025','valid-not-sa-024',
     'valid-not-sa-023','valid-not-sa-021','valid-not-sa-020',
     'valid-not-sa-019','valid-not-sa-018','valid-not-sa-017',
     'valid-not-sa-016','valid-not-sa-015','valid-not-sa-014',
     'valid-not-sa-013','valid-not-sa-012','valid-not-sa-011',
     'valid-not-sa-010','valid-not-sa-009','valid-not-sa-008',
     'valid-not-sa-007','valid-not-sa-006','valid-not-sa-005',
     'valid-not-sa-004','valid-not-sa-003','valid-not-sa-002',
     'valid-not-sa-001','valid-sa-119','valid-sa-118','valid-sa-117',
     'valid-sa-116','valid-sa-115','valid-sa-114','valid-sa-113',
     'valid-sa-112','valid-sa-111','valid-sa-110','valid-sa-109',
     'valid-sa-108','valid-sa-107','valid-sa-106','valid-sa-105',
     'valid-sa-104','valid-sa-103','valid-sa-102','valid-sa-101',
     'valid-sa-100','valid-sa-099','valid-sa-098','valid-sa-097',
     'valid-sa-096','valid-sa-095','valid-sa-094','valid-sa-093',
     'valid-sa-092','valid-sa-091','valid-sa-090','valid-sa-089',
     'valid-sa-088','valid-sa-087','valid-sa-086','valid-sa-085',
     'valid-sa-084','valid-sa-083','valid-sa-082','valid-sa-081',
     'valid-sa-080','valid-sa-079','valid-sa-078','valid-sa-077',
     'valid-sa-076','valid-sa-075','valid-sa-074','valid-sa-073',
     'valid-sa-072','valid-sa-071','valid-sa-070','valid-sa-069',
     'valid-sa-068','valid-sa-067','valid-sa-066','valid-sa-065',
     'valid-sa-064','valid-sa-063','valid-sa-062','valid-sa-061',
     'valid-sa-060','valid-sa-059','valid-sa-058','valid-sa-057',
     'valid-sa-056','valid-sa-055','valid-sa-054','valid-sa-053',
     'valid-sa-052','valid-sa-051','valid-sa-050','valid-sa-049',
     'valid-sa-048','valid-sa-047','valid-sa-046','valid-sa-045',
     'valid-sa-044','valid-sa-043','valid-sa-042','valid-sa-041',
     'valid-sa-040','valid-sa-039','valid-sa-038','valid-sa-037',
     'valid-sa-036','valid-sa-035','valid-sa-034','valid-sa-033',
     'valid-sa-032','valid-sa-031','valid-sa-030','valid-sa-029',
     'valid-sa-028','valid-sa-027','valid-sa-026','valid-sa-025',
     'valid-sa-024','valid-sa-023','valid-sa-022','valid-sa-021',
     'valid-sa-020','valid-sa-019','valid-sa-018','valid-sa-017',
     'valid-sa-016','valid-sa-015','valid-sa-014','valid-sa-013',
     'valid-sa-012','valid-sa-011','valid-sa-010','valid-sa-009',
     'valid-sa-008','valid-sa-007','valid-sa-006','valid-sa-005',
     'valid-sa-004','valid-sa-003','valid-sa-002','valid-sa-001',
     'invalid-not-sa-022','invalid--006','invalid--005','invalid--002',
     'not-wf-ext-sa-003','not-wf-ext-sa-002','not-wf-ext-sa-001',
     'not-wf-not-sa-009','not-wf-not-sa-008','not-wf-not-sa-007',
     'not-wf-not-sa-006','not-wf-not-sa-005','not-wf-not-sa-004',
     'not-wf-not-sa-003','not-wf-not-sa-002','not-wf-not-sa-001',
     'not-wf-sa-186','not-wf-sa-185','not-wf-sa-184','not-wf-sa-183',
     'not-wf-sa-182','not-wf-sa-181','not-wf-sa-180','not-wf-sa-179',
     'not-wf-sa-178','not-wf-sa-177','not-wf-sa-176','not-wf-sa-175',
     'not-wf-sa-174','not-wf-sa-173','not-wf-sa-172','not-wf-sa-171',
     'not-wf-sa-170','not-wf-sa-169','not-wf-sa-168','not-wf-sa-167',
     'not-wf-sa-166','not-wf-sa-165','not-wf-sa-164','not-wf-sa-163',
     'not-wf-sa-162','not-wf-sa-161','not-wf-sa-160','not-wf-sa-159',
     'not-wf-sa-158','not-wf-sa-157','not-wf-sa-156','not-wf-sa-155',
     'not-wf-sa-154','not-wf-sa-153','not-wf-sa-152','not-wf-sa-151',
     'not-wf-sa-150','not-wf-sa-149','not-wf-sa-148','not-wf-sa-147',
     'not-wf-sa-146','not-wf-sa-145','not-wf-sa-144','not-wf-sa-143',
     'not-wf-sa-142','not-wf-sa-141','not-wf-sa-140','not-wf-sa-139',
     'not-wf-sa-138','not-wf-sa-137','not-wf-sa-136','not-wf-sa-135',
     'not-wf-sa-134','not-wf-sa-133','not-wf-sa-132','not-wf-sa-131',
     'not-wf-sa-130','not-wf-sa-129','not-wf-sa-128','not-wf-sa-127',
     'not-wf-sa-126','not-wf-sa-125','not-wf-sa-124','not-wf-sa-123',
     'not-wf-sa-122','not-wf-sa-121','not-wf-sa-120','not-wf-sa-119',
     'not-wf-sa-118','not-wf-sa-117','not-wf-sa-116','not-wf-sa-115',
     'not-wf-sa-114','not-wf-sa-113','not-wf-sa-112','not-wf-sa-111',
     'not-wf-sa-110','not-wf-sa-109','not-wf-sa-108','not-wf-sa-107',
     'not-wf-sa-106','not-wf-sa-105','not-wf-sa-104','not-wf-sa-103',
     'not-wf-sa-102','not-wf-sa-101','not-wf-sa-100','not-wf-sa-099',
     'not-wf-sa-098','not-wf-sa-097','not-wf-sa-096','not-wf-sa-095',
     'not-wf-sa-094','not-wf-sa-093','not-wf-sa-092','not-wf-sa-091',
     'not-wf-sa-090','not-wf-sa-089','not-wf-sa-088','not-wf-sa-087',
     'not-wf-sa-086','not-wf-sa-085','not-wf-sa-084','not-wf-sa-083',
     'not-wf-sa-082','not-wf-sa-081','not-wf-sa-080','not-wf-sa-079',
     'not-wf-sa-078','not-wf-sa-077','not-wf-sa-076','not-wf-sa-075',
     'not-wf-sa-074','not-wf-sa-073','not-wf-sa-072','not-wf-sa-071',
     'not-wf-sa-070','not-wf-sa-069','not-wf-sa-068','not-wf-sa-067',
     'not-wf-sa-066','not-wf-sa-065','not-wf-sa-064','not-wf-sa-063',
     'not-wf-sa-062','not-wf-sa-061','not-wf-sa-060','not-wf-sa-059',
     'not-wf-sa-058','not-wf-sa-057','not-wf-sa-056','not-wf-sa-055',
     'not-wf-sa-054','not-wf-sa-053','not-wf-sa-052','not-wf-sa-051',
     'not-wf-sa-050','not-wf-sa-049','not-wf-sa-048','not-wf-sa-047',
     'not-wf-sa-046','not-wf-sa-045','not-wf-sa-044','not-wf-sa-043',
     'not-wf-sa-042','not-wf-sa-041','not-wf-sa-040','not-wf-sa-039',
     'not-wf-sa-038','not-wf-sa-037','not-wf-sa-036','not-wf-sa-035',
     'not-wf-sa-034','not-wf-sa-033','not-wf-sa-032','not-wf-sa-031',
     'not-wf-sa-030','not-wf-sa-029','not-wf-sa-028','not-wf-sa-027',
     'not-wf-sa-026','not-wf-sa-025','not-wf-sa-024','not-wf-sa-023',
     'not-wf-sa-022','not-wf-sa-021','not-wf-sa-020','not-wf-sa-019',
     'not-wf-sa-018','not-wf-sa-017','not-wf-sa-016','not-wf-sa-015',
     'not-wf-sa-014','not-wf-sa-013','not-wf-sa-012','not-wf-sa-011',
     'not-wf-sa-010','not-wf-sa-009','not-wf-sa-008','not-wf-sa-007',
     'not-wf-sa-006','not-wf-sa-005','not-wf-sa-004','not-wf-sa-003',
     'not-wf-sa-002','not-wf-sa-001'].
'xerox-japanese'(suite) -> 
    %% 12 test cases
    ['japanese-weekly-utf-8','japanese-weekly-utf-16',
     'japanese-weekly-shift_jis','japanese-weekly-little',
     'japanese-weekly-iso-2022-jp','japanese-weekly-euc-jp',
     'japanese-pr-xml-utf-8','japanese-pr-xml-utf-16',
     'japanese-pr-xml-shift_jis','japanese-pr-xml-little',
     'japanese-pr-xml-iso-2022-jp','japanese-pr-xml-euc-jp'].
'nist-oasis'(suite) -> 
    %% 348 test cases
    ['o-p11pass1','o-p76fail4','o-p76fail3','o-p76fail2','o-p76fail1',
     'o-p75fail6','o-p75fail5','o-p75fail4','o-p75fail3','o-p75fail2',
     'o-p75fail1','o-p74fail3','o-p74fail2','o-p74fail1','o-p73fail5',
     'o-p73fail4','o-p73fail3','o-p73fail2','o-p73fail1','o-p72fail4',
     'o-p72fail3','o-p72fail2','o-p72fail1','o-p71fail4','o-p71fail3',
     'o-p71fail2','o-p71fail1','o-p70fail1','o-p69fail3','o-p69fail2',
     'o-p69fail1','o-p68fail3','o-p68fail2','o-p68fail1','o-p66fail6',
     'o-p66fail5','o-p66fail4','o-p66fail3','o-p66fail2','o-p66fail1',
     'o-p64fail2','o-p64fail1','o-p63fail2','o-p63fail1','o-p62fail2',
     'o-p62fail1','o-p61fail1','o-p60fail5','o-p60fail4','o-p60fail3',
     'o-p60fail2','o-p60fail1','o-p59fail3','o-p59fail2','o-p59fail1',
     'o-p58fail8','o-p58fail7','o-p58fail6','o-p58fail5','o-p58fail4',
     'o-p58fail3','o-p58fail2','o-p58fail1','o-p57fail1','o-p56fail5',
     'o-p56fail4','o-p56fail3','o-p56fail2','o-p56fail1','o-p55fail1',
     'o-p54fail1','o-p53fail5','o-p53fail4','o-p53fail3','o-p53fail2',
     'o-p53fail1','o-p52fail2','o-p52fail1','o-p51fail7','o-p51fail6',
     'o-p51fail5','o-p51fail4','o-p51fail3','o-p51fail2','o-p51fail1',
     'o-p50fail1','o-p49fail1','o-p48fail2','o-p48fail1','o-p47fail4',
     'o-p47fail3','o-p47fail2','o-p47fail1','o-p46fail6','o-p46fail5',
     'o-p46fail4','o-p46fail3','o-p46fail2','o-p46fail1','o-p45fail4',
     'o-p45fail3','o-p45fail2','o-p45fail1','o-p44fail5','o-p44fail4',
     'o-p44fail3','o-p44fail2','o-p44fail1','o-p43fail3','o-p43fail2',
     'o-p43fail1','o-p42fail3','o-p42fail2','o-p42fail1','o-p41fail3',
     'o-p41fail2','o-p41fail1','o-p40fail4','o-p40fail3','o-p40fail2',
     'o-p40fail1','o-p39fail5','o-p39fail4','o-p39fail3','o-p39fail2',
     'o-p39fail1','o-p32fail5','o-p32fail4','o-p32fail3','o-p32fail2',
     'o-p32fail1','o-p31fail1','o-p30fail1','o-p29fail1','o-p28fail1',
     'o-p27fail1','o-p26fail2','o-p26fail1','o-p25fail1','o-p24fail2',
     'o-p24fail1','o-p23fail5','o-p23fail4','o-p23fail3','o-p23fail2',
     'o-p23fail1','o-p22fail2','o-p22fail1','o-p18fail3','o-p18fail2',
     'o-p18fail1','o-p16fail3','o-p16fail2','o-p16fail1','o-p15fail3',
     'o-p15fail2','o-p15fail1','o-p14fail3','o-p14fail2','o-p14fail1',
     'o-p12fail7','o-p12fail6','o-p12fail5','o-p12fail4','o-p12fail3',
     'o-p12fail2','o-p12fail1','o-p11fail2','o-p11fail1','o-p10fail3',
     'o-p10fail2','o-p10fail1','o-p09fail5','o-p09fail4','o-p09fail3',
     'o-p09fail2','o-p09fail1','o-p05fail5','o-p05fail4','o-p05fail3',
     'o-p05fail2','o-p05fail1','o-p04fail3','o-p04fail2','o-p04fail1',
     'o-p03fail9','o-p03fail8','o-p03fail7','o-p03fail5','o-p03fail4',
     'o-p03fail3','o-p03fail29','o-p03fail28','o-p03fail27','o-p03fail26',
     'o-p03fail25','o-p03fail24','o-p03fail23','o-p03fail22','o-p03fail21',
     'o-p03fail20','o-p03fail2','o-p03fail19','o-p03fail18','o-p03fail17',
     'o-p03fail16','o-p03fail15','o-p03fail14','o-p03fail13','o-p03fail12',
     'o-p03fail11','o-p03fail10','o-p03fail1','o-p02fail9','o-p02fail8',
     'o-p02fail7','o-p02fail6','o-p02fail5','o-p02fail4','o-p02fail31',
     'o-p02fail30','o-p02fail3','o-p02fail29','o-p02fail28','o-p02fail27',
     'o-p02fail26','o-p02fail25','o-p02fail24','o-p02fail23','o-p02fail22',
     'o-p02fail21','o-p02fail20','o-p02fail2','o-p02fail19','o-p02fail18',
     'o-p02fail17','o-p02fail16','o-p02fail15','o-p02fail14','o-p02fail13',
     'o-p02fail12','o-p02fail11','o-p02fail10','o-p02fail1','o-p01fail4',
     'o-p01fail3','o-p01fail2','o-p01fail1','o-e2','o-p75pass1',
     'o-p74pass1','o-p66pass1','o-p44pass5','o-p44pass4','o-p44pass3',
     'o-p44pass2','o-p44pass1','o-p42pass2','o-p42pass1','o-p41pass2',
     'o-p41pass1','o-p40pass4','o-p40pass3','o-p40pass2','o-p40pass1',
     'o-p39pass2','o-p39pass1','o-p32pass2','o-p32pass1','o-p27pass4',
     'o-p27pass3','o-p27pass2','o-p27pass1','o-p26pass1','o-p25pass2',
     'o-p25pass1','o-p24pass4','o-p24pass3','o-p24pass2','o-p24pass1',
     'o-p23pass4','o-p23pass3','o-p23pass2','o-p23pass1','o-p22pass3',
     'o-p22pass2','o-p22pass1','o-p18pass1','o-p16pass3','o-p16pass2',
     'o-p16pass1','o-p15pass1','o-p14pass1','o-p10pass1','o-p08fail2',
     'o-p08fail1','o-p06fail1','o-p05pass1','o-p04pass1','o-p03pass1',
     'o-p01pass3','o-p01pass1','o-p76pass1','o-p73pass1','o-p72pass1',
     'o-p71pass1','o-p70pass1','o-p69pass1','o-p68pass1','o-p64pass1',
     'o-p63pass1','o-p62pass1','o-p61pass1','o-p60pass1','o-p59pass1',
     'o-p58pass1','o-p57pass1','o-p56pass1','o-p55pass1','o-p54pass1',
     'o-p53pass1','o-p52pass1','o-p51pass1','o-p50pass1','o-p49pass1',
     'o-p48pass1','o-p47pass1','o-p46pass1','o-p45pass1','o-p43pass1',
     'o-p31pass2','o-p31pass1','o-p30pass2','o-p30pass1','o-p29pass1',
     'o-p28pass5','o-p28pass4','o-p28pass3','o-p28pass1','o-p22pass6',
     'o-p22pass5','o-p22pass4','o-p12pass1','o-p09pass1','o-p08pass1',
     'o-p07pass1','o-p06pass1','o-p01pass2'].
'ibm-invalid'(suite) -> 
    %% 48 test cases
    ['ibm-invalid-P76-ibm76i01','ibm-invalid-P69-ibm69i04',
     'ibm-invalid-P69-ibm69i03','ibm-invalid-P69-ibm69i02',
     'ibm-invalid-P69-ibm69i01','ibm-invalid-P68-ibm68i04',
     'ibm-invalid-P68-ibm68i03','ibm-invalid-P68-ibm68i02',
     'ibm-invalid-P68-ibm68i01','ibm-invalid-P60-ibm60i04',
     'ibm-invalid-P60-ibm60i03','ibm-invalid-P60-ibm60i02',
     'ibm-invalid-P60-ibm60i01','ibm-invalid-P59-ibm59i01',
     'ibm-invalid-P58-ibm58i02','ibm-invalid-P58-ibm58i01',
     'ibm-invalid-P56-ibm56i18','ibm-invalid-P56-ibm56i17',
     'ibm-invalid-P56-ibm56i16','ibm-invalid-P56-ibm56i15',
     'ibm-invalid-P56-ibm56i14','ibm-invalid-P56-ibm56i13',
     'ibm-invalid-P56-ibm56i12','ibm-invalid-P56-ibm56i11',
     'ibm-invalid-P56-ibm56i10','ibm-invalid-P56-ibm56i09',
     'ibm-invalid-P56-ibm56i08','ibm-invalid-P56-ibm56i07',
     'ibm-invalid-P56-ibm56i06','ibm-invalid-P56-ibm56i05',
     'ibm-invalid-P56-ibm56i03','ibm-invalid-P56-ibm56i02',
     'ibm-invalid-P56-ibm56i01','ibm-invalid-P51-ibm51i03',
     'ibm-invalid-P51-ibm51i01','ibm-invalid-P50-ibm50i01',
     'ibm-invalid-P49-ibm49i01','ibm-invalid-P45-ibm45i01',
     'ibm-invalid-P41-ibm41i02','ibm-invalid-P41-ibm41i01',
     'ibm-invalid-P39-ibm39i04','ibm-invalid-P39-ibm39i03',
     'ibm-invalid-P39-ibm39i02','ibm-invalid-P39-ibm39i01',
     'ibm-invalid-P32-ibm32i04','ibm-invalid-P32-ibm32i03',
     'ibm-invalid-P32-ibm32i01','ibm-invalid-P28-ibm28i01'].
'ibm-not-wf'(suite) -> 
    %% 731 test cases
    ['ibm-not-wf-P89-ibm89n12','ibm-not-wf-P89-ibm89n11',
     'ibm-not-wf-P89-ibm89n10','ibm-not-wf-P89-ibm89n09',
     'ibm-not-wf-P89-ibm89n08','ibm-not-wf-P89-ibm89n07',
     'ibm-not-wf-P89-ibm89n06','ibm-not-wf-P89-ibm89n05',
     'ibm-not-wf-P89-ibm89n04','ibm-not-wf-P89-ibm89n03',
     'ibm-not-wf-P89-ibm89n02','ibm-not-wf-P89-ibm89n01',
     'ibm-not-wf-P88-ibm88n16','ibm-not-wf-P88-ibm88n15',
     'ibm-not-wf-P88-ibm88n14','ibm-not-wf-P88-ibm88n13',
     'ibm-not-wf-P88-ibm88n12','ibm-not-wf-P88-ibm88n11',
     'ibm-not-wf-P88-ibm88n10','ibm-not-wf-P88-ibm88n09',
     'ibm-not-wf-P88-ibm88n08','ibm-not-wf-P88-ibm88n06',
     'ibm-not-wf-P88-ibm88n05','ibm-not-wf-P88-ibm88n04',
     'ibm-not-wf-P88-ibm88n03','ibm-not-wf-P88-ibm88n02',
     'ibm-not-wf-P88-ibm88n01','ibm-not-wf-P87-ibm87n85',
     'ibm-not-wf-P87-ibm87n84','ibm-not-wf-P87-ibm87n83',
     'ibm-not-wf-P87-ibm87n82','ibm-not-wf-P87-ibm87n81',
     'ibm-not-wf-P87-ibm87n80','ibm-not-wf-P87-ibm87n79',
     'ibm-not-wf-P87-ibm87n78','ibm-not-wf-P87-ibm87n77',
     'ibm-not-wf-P87-ibm87n76','ibm-not-wf-P87-ibm87n75',
     'ibm-not-wf-P87-ibm87n74','ibm-not-wf-P87-ibm87n73',
     'ibm-not-wf-P87-ibm87n72','ibm-not-wf-P87-ibm87n71',
     'ibm-not-wf-P87-ibm87n70','ibm-not-wf-P87-ibm87n69',
     'ibm-not-wf-P87-ibm87n68','ibm-not-wf-P87-ibm87n67',
     'ibm-not-wf-P87-ibm87n66','ibm-not-wf-P87-ibm87n64',
     'ibm-not-wf-P87-ibm87n63','ibm-not-wf-P87-ibm87n62',
     'ibm-not-wf-P87-ibm87n61','ibm-not-wf-P87-ibm87n60',
     'ibm-not-wf-P87-ibm87n59','ibm-not-wf-P87-ibm87n58',
     'ibm-not-wf-P87-ibm87n57','ibm-not-wf-P87-ibm87n56',
     'ibm-not-wf-P87-ibm87n55','ibm-not-wf-P87-ibm87n54',
     'ibm-not-wf-P87-ibm87n53','ibm-not-wf-P87-ibm87n52',
     'ibm-not-wf-P87-ibm87n51','ibm-not-wf-P87-ibm87n50',
     'ibm-not-wf-P87-ibm87n49','ibm-not-wf-P87-ibm87n48',
     'ibm-not-wf-P87-ibm87n47','ibm-not-wf-P87-ibm87n46',
     'ibm-not-wf-P87-ibm87n45','ibm-not-wf-P87-ibm87n44',
     'ibm-not-wf-P87-ibm87n43','ibm-not-wf-P87-ibm87n42',
     'ibm-not-wf-P87-ibm87n41','ibm-not-wf-P87-ibm87n40',
     'ibm-not-wf-P87-ibm87n39','ibm-not-wf-P87-ibm87n38',
     'ibm-not-wf-P87-ibm87n37','ibm-not-wf-P87-ibm87n36',
     'ibm-not-wf-P87-ibm87n35','ibm-not-wf-P87-ibm87n34',
     'ibm-not-wf-P87-ibm87n33','ibm-not-wf-P87-ibm87n32',
     'ibm-not-wf-P87-ibm87n31','ibm-not-wf-P87-ibm87n30',
     'ibm-not-wf-P87-ibm87n29','ibm-not-wf-P87-ibm87n28',
     'ibm-not-wf-P87-ibm87n27','ibm-not-wf-P87-ibm87n26',
     'ibm-not-wf-P87-ibm87n25','ibm-not-wf-P87-ibm87n24',
     'ibm-not-wf-P87-ibm87n23','ibm-not-wf-P87-ibm87n22',
     'ibm-not-wf-P87-ibm87n21','ibm-not-wf-P87-ibm87n20',
     'ibm-not-wf-P87-ibm87n19','ibm-not-wf-P87-ibm87n18',
     'ibm-not-wf-P87-ibm87n17','ibm-not-wf-P87-ibm87n16',
     'ibm-not-wf-P87-ibm87n15','ibm-not-wf-P87-ibm87n14',
     'ibm-not-wf-P87-ibm87n13','ibm-not-wf-P87-ibm87n12',
     'ibm-not-wf-P87-ibm87n11','ibm-not-wf-P87-ibm87n10',
     'ibm-not-wf-P87-ibm87n09','ibm-not-wf-P87-ibm87n08',
     'ibm-not-wf-P87-ibm87n07','ibm-not-wf-P87-ibm87n06',
     'ibm-not-wf-P87-ibm87n05','ibm-not-wf-P87-ibm87n04',
     'ibm-not-wf-P87-ibm87n03','ibm-not-wf-P87-ibm87n02',
     'ibm-not-wf-P87-ibm87n01','ibm-not-wf-P86-ibm86n04',
     'ibm-not-wf-P86-ibm86n03','ibm-not-wf-P86-ibm86n02',
     'ibm-not-wf-P86-ibm86n01','ibm-not-wf-P85-ibm85n99',
     'ibm-not-wf-P85-ibm85n98','ibm-not-wf-P85-ibm85n97',
     'ibm-not-wf-P85-ibm85n96','ibm-not-wf-P85-ibm85n95',
     'ibm-not-wf-P85-ibm85n94','ibm-not-wf-P85-ibm85n93',
     'ibm-not-wf-P85-ibm85n92','ibm-not-wf-P85-ibm85n91',
     'ibm-not-wf-P85-ibm85n90','ibm-not-wf-P85-ibm85n89',
     'ibm-not-wf-P85-ibm85n88','ibm-not-wf-P85-ibm85n87',
     'ibm-not-wf-P85-ibm85n86','ibm-not-wf-P85-ibm85n85',
     'ibm-not-wf-P85-ibm85n84','ibm-not-wf-P85-ibm85n83',
     'ibm-not-wf-P85-ibm85n82','ibm-not-wf-P85-ibm85n81',
     'ibm-not-wf-P85-ibm85n80','ibm-not-wf-P85-ibm85n79',
     'ibm-not-wf-P85-ibm85n78','ibm-not-wf-P85-ibm85n77',
     'ibm-not-wf-P85-ibm85n76','ibm-not-wf-P85-ibm85n75',
     'ibm-not-wf-P85-ibm85n74','ibm-not-wf-P85-ibm85n73',
     'ibm-not-wf-P85-ibm85n72','ibm-not-wf-P85-ibm85n71',
     'ibm-not-wf-P85-ibm85n70','ibm-not-wf-P85-ibm85n69',
     'ibm-not-wf-P85-ibm85n68','ibm-not-wf-P85-ibm85n67',
     'ibm-not-wf-P85-ibm85n66','ibm-not-wf-P85-ibm85n65',
     'ibm-not-wf-P85-ibm85n64','ibm-not-wf-P85-ibm85n63',
     'ibm-not-wf-P85-ibm85n62','ibm-not-wf-P85-ibm85n61',
     'ibm-not-wf-P85-ibm85n60','ibm-not-wf-P85-ibm85n59',
     'ibm-not-wf-P85-ibm85n58','ibm-not-wf-P85-ibm85n57',
     'ibm-not-wf-P85-ibm85n56','ibm-not-wf-P85-ibm85n55',
     'ibm-not-wf-P85-ibm85n54','ibm-not-wf-P85-ibm85n53',
     'ibm-not-wf-P85-ibm85n52','ibm-not-wf-P85-ibm85n51',
     'ibm-not-wf-P85-ibm85n50','ibm-not-wf-P85-ibm85n49',
     'ibm-not-wf-P85-ibm85n48','ibm-not-wf-P85-ibm85n47',
     'ibm-not-wf-P85-ibm85n46','ibm-not-wf-P85-ibm85n45',
     'ibm-not-wf-P85-ibm85n44','ibm-not-wf-P85-ibm85n43',
     'ibm-not-wf-P85-ibm85n42','ibm-not-wf-P85-ibm85n41',
     'ibm-not-wf-P85-ibm85n40','ibm-not-wf-P85-ibm85n39',
     'ibm-not-wf-P85-ibm85n38','ibm-not-wf-P85-ibm85n37',
     'ibm-not-wf-P85-ibm85n36','ibm-not-wf-P85-ibm85n35',
     'ibm-not-wf-P85-ibm85n34','ibm-not-wf-P85-ibm85n33',
     'ibm-not-wf-P85-ibm85n32','ibm-not-wf-P85-ibm85n31',
     'ibm-not-wf-P85-ibm85n30','ibm-not-wf-P85-ibm85n29',
     'ibm-not-wf-P85-ibm85n28','ibm-not-wf-P85-ibm85n27',
     'ibm-not-wf-P85-ibm85n26','ibm-not-wf-P85-ibm85n25',
     'ibm-not-wf-P85-ibm85n24','ibm-not-wf-P85-ibm85n23',
     'ibm-not-wf-P85-ibm85n22','ibm-not-wf-P85-ibm85n21',
     'ibm-not-wf-P85-ibm85n20','ibm-not-wf-P85-ibm85n198',
     'ibm-not-wf-P85-ibm85n197','ibm-not-wf-P85-ibm85n196',
     'ibm-not-wf-P85-ibm85n195','ibm-not-wf-P85-ibm85n194',
     'ibm-not-wf-P85-ibm85n193','ibm-not-wf-P85-ibm85n192',
     'ibm-not-wf-P85-ibm85n191','ibm-not-wf-P85-ibm85n190',
     'ibm-not-wf-P85-ibm85n19','ibm-not-wf-P85-ibm85n189',
     'ibm-not-wf-P85-ibm85n188','ibm-not-wf-P85-ibm85n187',
     'ibm-not-wf-P85-ibm85n186','ibm-not-wf-P85-ibm85n185',
     'ibm-not-wf-P85-ibm85n184','ibm-not-wf-P85-ibm85n183',
     'ibm-not-wf-P85-ibm85n182','ibm-not-wf-P85-ibm85n181',
     'ibm-not-wf-P85-ibm85n180','ibm-not-wf-P85-ibm85n18',
     'ibm-not-wf-P85-ibm85n179','ibm-not-wf-P85-ibm85n178',
     'ibm-not-wf-P85-ibm85n177','ibm-not-wf-P85-ibm85n176',
     'ibm-not-wf-P85-ibm85n175','ibm-not-wf-P85-ibm85n174',
     'ibm-not-wf-P85-ibm85n173','ibm-not-wf-P85-ibm85n172',
     'ibm-not-wf-P85-ibm85n171','ibm-not-wf-P85-ibm85n170',
     'ibm-not-wf-P85-ibm85n17','ibm-not-wf-P85-ibm85n169',
     'ibm-not-wf-P85-ibm85n168','ibm-not-wf-P85-ibm85n167',
     'ibm-not-wf-P85-ibm85n166','ibm-not-wf-P85-ibm85n165',
     'ibm-not-wf-P85-ibm85n164','ibm-not-wf-P85-ibm85n163',
     'ibm-not-wf-P85-ibm85n162','ibm-not-wf-P85-ibm85n161',
     'ibm-not-wf-P85-ibm85n160','ibm-not-wf-P85-ibm85n16',
     'ibm-not-wf-P85-ibm85n159','ibm-not-wf-P85-ibm85n158',
     'ibm-not-wf-P85-ibm85n157','ibm-not-wf-P85-ibm85n156',
     'ibm-not-wf-P85-ibm85n155','ibm-not-wf-P85-ibm85n154',
     'ibm-not-wf-P85-ibm85n153','ibm-not-wf-P85-ibm85n152',
     'ibm-not-wf-P85-ibm85n151','ibm-not-wf-P85-ibm85n150',
     'ibm-not-wf-P85-ibm85n15','ibm-not-wf-P85-ibm85n149',
     'ibm-not-wf-P85-ibm85n148','ibm-not-wf-P85-ibm85n147',
     'ibm-not-wf-P85-ibm85n146','ibm-not-wf-P85-ibm85n145',
     'ibm-not-wf-P85-ibm85n144','ibm-not-wf-P85-ibm85n143',
     'ibm-not-wf-P85-ibm85n142','ibm-not-wf-P85-ibm85n141',
     'ibm-not-wf-P85-ibm85n140','ibm-not-wf-P85-ibm85n14',
     'ibm-not-wf-P85-ibm85n139','ibm-not-wf-P85-ibm85n138',
     'ibm-not-wf-P85-ibm85n137','ibm-not-wf-P85-ibm85n136',
     'ibm-not-wf-P85-ibm85n135','ibm-not-wf-P85-ibm85n134',
     'ibm-not-wf-P85-ibm85n133','ibm-not-wf-P85-ibm85n132',
     'ibm-not-wf-P85-ibm85n131','ibm-not-wf-P85-ibm85n130',
     'ibm-not-wf-P85-ibm85n13','ibm-not-wf-P85-ibm85n129',
     'ibm-not-wf-P85-ibm85n128','ibm-not-wf-P85-ibm85n127',
     'ibm-not-wf-P85-ibm85n126','ibm-not-wf-P85-ibm85n125',
     'ibm-not-wf-P85-ibm85n124','ibm-not-wf-P85-ibm85n123',
     'ibm-not-wf-P85-ibm85n122','ibm-not-wf-P85-ibm85n121',
     'ibm-not-wf-P85-ibm85n120','ibm-not-wf-P85-ibm85n12',
     'ibm-not-wf-P85-ibm85n119','ibm-not-wf-P85-ibm85n118',
     'ibm-not-wf-P85-ibm85n117','ibm-not-wf-P85-ibm85n116',
     'ibm-not-wf-P85-ibm85n115','ibm-not-wf-P85-ibm85n114',
     'ibm-not-wf-P85-ibm85n113','ibm-not-wf-P85-ibm85n112',
     'ibm-not-wf-P85-ibm85n111','ibm-not-wf-P85-ibm85n110',
     'ibm-not-wf-P85-ibm85n11','ibm-not-wf-P85-ibm85n109',
     'ibm-not-wf-P85-ibm85n108','ibm-not-wf-P85-ibm85n107',
     'ibm-not-wf-P85-ibm85n106','ibm-not-wf-P85-ibm85n105',
     'ibm-not-wf-P85-ibm85n104','ibm-not-wf-P85-ibm85n103',
     'ibm-not-wf-P85-ibm85n102','ibm-not-wf-P85-ibm85n101',
     'ibm-not-wf-P85-ibm85n100','ibm-not-wf-P85-ibm85n10',
     'ibm-not-wf-P85-ibm85n09','ibm-not-wf-P85-ibm85n08',
     'ibm-not-wf-P85-ibm85n07','ibm-not-wf-P85-ibm85n06',
     'ibm-not-wf-P85-ibm85n05','ibm-not-wf-P85-ibm85n04',
     'ibm-not-wf-P85-ibm85n03','ibm-not-wf-P85-ibm85n02',
     'ibm-not-wf-P85-ibm85n01','ibm-not-wf-P83-ibm83n06',
     'ibm-not-wf-P83-ibm83n05','ibm-not-wf-P83-ibm83n04',
     'ibm-not-wf-P83-ibm83n03','ibm-not-wf-P83-ibm83n02',
     'ibm-not-wf-P83-ibm83n01','ibm-not-wf-P82-ibm82n08',
     'ibm-not-wf-P82-ibm82n07','ibm-not-wf-P82-ibm82n06',
     'ibm-not-wf-P82-ibm82n05','ibm-not-wf-P82-ibm82n04',
     'ibm-not-wf-P82-ibm82n03','ibm-not-wf-P82-ibm82n02',
     'ibm-not-wf-P82-ibm82n01','ibm-not-wf-P81-ibm81n09',
     'ibm-not-wf-P81-ibm81n08','ibm-not-wf-P81-ibm81n07',
     'ibm-not-wf-P81-ibm81n06','ibm-not-wf-P81-ibm81n05',
     'ibm-not-wf-P81-ibm81n04','ibm-not-wf-P81-ibm81n03',
     'ibm-not-wf-P81-ibm81n02','ibm-not-wf-P81-ibm81n01',
     'ibm-not-wf-P80-ibm80n06','ibm-not-wf-P80-ibm80n05',
     'ibm-not-wf-P80-ibm80n04','ibm-not-wf-P80-ibm80n03',
     'ibm-not-wf-P80-ibm80n02','ibm-not-wf-P80-ibm80n01',
     'ibm-not-wf-P79-ibm79n02','ibm-not-wf-P79-ibm79n01',
     'ibm-not-wf-P78-ibm78n02','ibm-not-wf-P78-ibm78n01',
     'ibm-not-wf-P77-ibm77n04','ibm-not-wf-P77-ibm77n03',
     'ibm-not-wf-P77-ibm77n02','ibm-not-wf-P77-ibm77n01',
     'ibm-not-wf-P76-ibm76n07','ibm-not-wf-P76-ibm76n06',
     'ibm-not-wf-P76-ibm76n05','ibm-not-wf-P76-ibm76n04',
     'ibm-not-wf-P76-ibm76n03','ibm-not-wf-P76-ibm76n02',
     'ibm-not-wf-P76-ibm76n01','ibm-not-wf-P75-ibm75n13',
     'ibm-not-wf-P75-ibm75n12','ibm-not-wf-P75-ibm75n11',
     'ibm-not-wf-P75-ibm75n10','ibm-not-wf-P75-ibm75n09',
     'ibm-not-wf-P75-ibm75n08','ibm-not-wf-P75-ibm75n07',
     'ibm-not-wf-P75-ibm75n06','ibm-not-wf-P75-ibm75n05',
     'ibm-not-wf-P75-ibm75n04','ibm-not-wf-P75-ibm75n03',
     'ibm-not-wf-P75-ibm75n02','ibm-not-wf-P75-ibm75n01',
     'ibm-not-wf-P74-ibm74n01','ibm-not-wf-P73-ibm73n03',
     'ibm-not-wf-P73-ibm73n01','ibm-not-wf-P72-ibm72n09',
     'ibm-not-wf-P72-ibm72n08','ibm-not-wf-P72-ibm72n07',
     'ibm-not-wf-P72-ibm72n06','ibm-not-wf-P72-ibm72n05',
     'ibm-not-wf-P72-ibm72n04','ibm-not-wf-P72-ibm72n03',
     'ibm-not-wf-P72-ibm72n02','ibm-not-wf-P72-ibm72n01',
     'ibm-not-wf-P71-ibm71n08','ibm-not-wf-P71-ibm71n07',
     'ibm-not-wf-P71-ibm71n06','ibm-not-wf-P71-ibm71n05',
     'ibm-not-wf-P71-ibm71n04','ibm-not-wf-P71-ibm71n03',
     'ibm-not-wf-P71-ibm71n02','ibm-not-wf-P71-ibm71n01',
     'ibm-not-wf-P71-ibm70n01','ibm-not-wf-P69-ibm69n07',
     'ibm-not-wf-P69-ibm69n06','ibm-not-wf-P69-ibm69n05',
     'ibm-not-wf-P69-ibm69n04','ibm-not-wf-P69-ibm69n03',
     'ibm-not-wf-P69-ibm69n02','ibm-not-wf-P69-ibm69n01',
     'ibm-not-wf-P68-ibm68n10','ibm-not-wf-P68-ibm68n09',
     'ibm-not-wf-P68-ibm68n08','ibm-not-wf-P68-ibm68n07',
     'ibm-not-wf-P68-ibm68n06','ibm-not-wf-P68-ibm68n05',
     'ibm-not-wf-P68-ibm68n04','ibm-not-wf-P68-ibm68n03',
     'ibm-not-wf-P68-ibm68n02','ibm-not-wf-P68-ibm68n01',
     'ibm-not-wf-P66-ibm66n15','ibm-not-wf-P66-ibm66n14',
     'ibm-not-wf-P66-ibm66n13','ibm-not-wf-P66-ibm66n12',
     'ibm-not-wf-P66-ibm66n11','ibm-not-wf-P66-ibm66n10',
     'ibm-not-wf-P66-ibm66n09','ibm-not-wf-P66-ibm66n08',
     'ibm-not-wf-P66-ibm66n07','ibm-not-wf-P66-ibm66n06',
     'ibm-not-wf-P66-ibm66n05','ibm-not-wf-P66-ibm66n04',
     'ibm-not-wf-P66-ibm66n03','ibm-not-wf-P66-ibm66n02',
     'ibm-not-wf-P66-ibm66n01','ibm-not-wf-P65-ibm65n02',
     'ibm-not-wf-P65-ibm65n01','ibm-not-wf-P64-ibm64n03',
     'ibm-not-wf-P64-ibm64n02','ibm-not-wf-P64-ibm64n01',
     'ibm-not-wf-P63-ibm63n07','ibm-not-wf-P63-ibm63n06',
     'ibm-not-wf-P63-ibm63n05','ibm-not-wf-P63-ibm63n04',
     'ibm-not-wf-P63-ibm63n03','ibm-not-wf-P63-ibm63n02',
     'ibm-not-wf-P63-ibm63n01','ibm-not-wf-P62-ibm62n08',
     'ibm-not-wf-P62-ibm62n07','ibm-not-wf-P62-ibm62n06',
     'ibm-not-wf-P62-ibm62n05','ibm-not-wf-P62-ibm62n04',
     'ibm-not-wf-P62-ibm62n03','ibm-not-wf-P62-ibm62n02',
     'ibm-not-wf-P62-ibm62n01','ibm-not-wf-P61-ibm61n01',
     'ibm-not-wf-P60-ibm60n08','ibm-not-wf-P60-ibm60n07',
     'ibm-not-wf-P60-ibm60n06','ibm-not-wf-P60-ibm60n05',
     'ibm-not-wf-P60-ibm60n04','ibm-not-wf-P60-ibm60n03',
     'ibm-not-wf-P60-ibm60n02','ibm-not-wf-P60-ibm60n01',
     'ibm-not-wf-P59-ibm59n06','ibm-not-wf-P59-ibm59n05',
     'ibm-not-wf-P59-ibm59n04','ibm-not-wf-P59-ibm59n03',
     'ibm-not-wf-P59-ibm59n02','ibm-not-wf-P59-ibm59n01',
     'ibm-not-wf-P58-ibm58n08','ibm-not-wf-P58-ibm58n07',
     'ibm-not-wf-P58-ibm58n06','ibm-not-wf-P58-ibm58n05',
     'ibm-not-wf-P58-ibm58n04','ibm-not-wf-P58-ibm58n03',
     'ibm-not-wf-P58-ibm58n02','ibm-not-wf-P58-ibm58n01',
     'ibm-not-wf-P57-ibm57n01','ibm-not-wf-P56-ibm56n07',
     'ibm-not-wf-P56-ibm56n06','ibm-not-wf-P56-ibm56n05',
     'ibm-not-wf-P56-ibm56n04','ibm-not-wf-P56-ibm56n03',
     'ibm-not-wf-P56-ibm56n02','ibm-not-wf-P56-ibm56n01',
     'ibm-not-wf-P55-ibm55n03','ibm-not-wf-P55-ibm55n02',
     'ibm-not-wf-P55-ibm55n01','ibm-not-wf-P54-ibm54n02',
     'ibm-not-wf-P54-ibm54n01','ibm-not-wf-P53-ibm53n08',
     'ibm-not-wf-P53-ibm53n07','ibm-not-wf-P53-ibm53n06',
     'ibm-not-wf-P53-ibm53n05','ibm-not-wf-P53-ibm53n04',
     'ibm-not-wf-P53-ibm53n03','ibm-not-wf-P53-ibm53n02',
     'ibm-not-wf-P53-ibm53n01','ibm-not-wf-P52-ibm52n06',
     'ibm-not-wf-P52-ibm52n05','ibm-not-wf-P52-ibm52n04',
     'ibm-not-wf-P52-ibm52n03','ibm-not-wf-P52-ibm52n02',
     'ibm-not-wf-P52-ibm52n01','ibm-not-wf-P51-ibm51n07',
     'ibm-not-wf-P51-ibm51n06','ibm-not-wf-P51-ibm51n05',
     'ibm-not-wf-P51-ibm51n04','ibm-not-wf-P51-ibm51n03',
     'ibm-not-wf-P51-ibm51n02','ibm-not-wf-P51-ibm51n01',
     'ibm-not-wf-P50-ibm50n07','ibm-not-wf-P50-ibm50n06',
     'ibm-not-wf-P50-ibm50n05','ibm-not-wf-P50-ibm50n04',
     'ibm-not-wf-P50-ibm50n03','ibm-not-wf-P50-ibm50n02',
     'ibm-not-wf-P50-ibm50n01','ibm-not-wf-P49-ibm49n06',
     'ibm-not-wf-P49-ibm49n05','ibm-not-wf-P49-ibm49n04',
     'ibm-not-wf-P49-ibm49n03','ibm-not-wf-P49-ibm49n02',
     'ibm-not-wf-P49-ibm49n01','ibm-not-wf-P48-ibm48n07',
     'ibm-not-wf-P48-ibm48n06','ibm-not-wf-P48-ibm48n05',
     'ibm-not-wf-P48-ibm48n04','ibm-not-wf-P48-ibm48n03',
     'ibm-not-wf-P48-ibm48n02','ibm-not-wf-P48-ibm48n01',
     'ibm-not-wf-P47-ibm47n06','ibm-not-wf-P47-ibm47n05',
     'ibm-not-wf-P47-ibm47n04','ibm-not-wf-P47-ibm47n03',
     'ibm-not-wf-P47-ibm47n02','ibm-not-wf-P47-ibm47n01',
     'ibm-not-wf-P46-ibm46n05','ibm-not-wf-P46-ibm46n04',
     'ibm-not-wf-P46-ibm46n03','ibm-not-wf-P46-ibm46n02',
     'ibm-not-wf-P46-ibm46n01','ibm-not-wf-P45-ibm45n09',
     'ibm-not-wf-P45-ibm45n08','ibm-not-wf-P45-ibm45n07',
     'ibm-not-wf-P45-ibm45n06','ibm-not-wf-P45-ibm45n05',
     'ibm-not-wf-P45-ibm45n04','ibm-not-wf-P45-ibm45n03',
     'ibm-not-wf-P45-ibm45n02','ibm-not-wf-P45-ibm45n01',
     'ibm-not-wf-P44-ibm44n04','ibm-not-wf-P44-ibm44n03',
     'ibm-not-wf-P44-ibm44n02','ibm-not-wf-P44-ibm44n01',
     'ibm-not-wf-P43-ibm43n05','ibm-not-wf-P43-ibm43n04',
     'ibm-not-wf-P43-ibm43n02','ibm-not-wf-P43-ibm43n01',
     'ibm-not-wf-P42-ibm42n05','ibm-not-wf-P42-ibm42n04',
     'ibm-not-wf-P42-ibm42n03','ibm-not-wf-P42-ibm42n02',
     'ibm-not-wf-P42-ibm42n01','ibm-not-wf-P41-ibm41n14',
     'ibm-not-wf-P41-ibm41n13','ibm-not-wf-P41-ibm41n12',
     'ibm-not-wf-P41-ibm41n11','ibm-not-wf-P41-ibm41n10',
     'ibm-not-wf-P41-ibm41n09','ibm-not-wf-P41-ibm41n08',
     'ibm-not-wf-P41-ibm41n07','ibm-not-wf-P41-ibm41n06',
     'ibm-not-wf-P41-ibm41n05','ibm-not-wf-P41-ibm41n04',
     'ibm-not-wf-P41-ibm41n03','ibm-not-wf-P41-ibm41n02',
     'ibm-not-wf-P41-ibm41n01','ibm-not-wf-P40-ibm40n05',
     'ibm-not-wf-P40-ibm40n04','ibm-not-wf-P40-ibm40n03',
     'ibm-not-wf-P40-ibm40n02','ibm-not-wf-P40-ibm40n01',
     'ibm-not-wf-P39-ibm39n06','ibm-not-wf-P39-ibm39n05',
     'ibm-not-wf-P39-ibm39n04','ibm-not-wf-P39-ibm39n03',
     'ibm-not-wf-P39-ibm39n02','ibm-not-wf-P39-ibm39n01',
     'ibm-not-wf-P32-ibm32n09','ibm-not-wf-P32-ibm32n08',
     'ibm-not-wf-P32-ibm32n07','ibm-not-wf-P32-ibm32n06',
     'ibm-not-wf-P32-ibm32n05','ibm-not-wf-P32-ibm32n04',
     'ibm-not-wf-P32-ibm32n03','ibm-not-wf-P32-ibm32n02',
     'ibm-not-wf-P32-ibm32n01','ibm-not-wf-P31-ibm31n01',
     'ibm-not-wf-P30-ibm30n01','ibm-not-wf-P29-ibm29n07',
     'ibm-not-wf-P29-ibm29n06','ibm-not-wf-P29-ibm29n05',
     'ibm-not-wf-P29-ibm29n04','ibm-not-wf-P29-ibm29n03',
     'ibm-not-wf-P29-ibm29n02','ibm-not-wf-P29-ibm29n01',
     'ibm-not-wf-P28a-ibm28an01','ibm-not-wf-P28-ibm28n08',
     'ibm-not-wf-P28-ibm28n07','ibm-not-wf-P28-ibm28n06',
     'ibm-not-wf-P28-ibm28n05','ibm-not-wf-P28-ibm28n04',
     'ibm-not-wf-P28-ibm28n03','ibm-not-wf-P28-ibm28n02',
     'ibm-not-wf-P28-ibm28n01','ibm-not-wf-P27-ibm27n01',
     'ibm-not-wf-P26-ibm26n01','ibm-not-wf-P25-ibm25n02',
     'ibm-not-wf-P25-ibm25n01','ibm-not-wf-P24-ibm24n09',
     'ibm-not-wf-P24-ibm24n08','ibm-not-wf-P24-ibm24n07',
     'ibm-not-wf-P24-ibm24n06','ibm-not-wf-P24-ibm24n05',
     'ibm-not-wf-P24-ibm24n04','ibm-not-wf-P24-ibm24n03',
     'ibm-not-wf-P24-ibm24n02','ibm-not-wf-P24-ibm24n01',
     'ibm-not-wf-P23-ibm23n06','ibm-not-wf-P23-ibm23n05',
     'ibm-not-wf-P23-ibm23n04','ibm-not-wf-P23-ibm23n03',
     'ibm-not-wf-P23-ibm23n02','ibm-not-wf-P23-ibm23n01',
     'ibm-not-wf-P22-ibm22n03','ibm-not-wf-P22-ibm22n02',
     'ibm-not-wf-P22-ibm22n01','ibm-not-wf-P21-ibm21n03',
     'ibm-not-wf-P21-ibm21n02','ibm-not-wf-P21-ibm21n01',
     'ibm-not-wf-P20-ibm20n01','ibm-not-wf-P19-ibm19n03',
     'ibm-not-wf-P19-ibm19n02','ibm-not-wf-P19-ibm19n01',
     'ibm-not-wf-P18-ibm18n02','ibm-not-wf-P18-ibm18n01',
     'ibm-not-wf-P17-ibm17n04','ibm-not-wf-P17-ibm17n03',
     'ibm-not-wf-P17-ibm17n02','ibm-not-wf-P17-ibm17n01',
     'ibm-not-wf-P16-ibm16n04','ibm-not-wf-P16-ibm16n03',
     'ibm-not-wf-P16-ibm16n02','ibm-not-wf-P16-ibm16n01',
     'ibm-not-wf-P15-ibm15n04','ibm-not-wf-P15-ibm15n03',
     'ibm-not-wf-P15-ibm15n02','ibm-not-wf-P15-ibm15n01',
     'ibm-not-wf-P14-ibm14n03','ibm-not-wf-P14-ibm14n02',
     'ibm-not-wf-P14-ibm14n01','ibm-not-wf-P13-ibm13n03',
     'ibm-not-wf-P13-ibm13n02','ibm-not-wf-P13-ibm13n01',
     'ibm-not-wf-P12-ibm12n03','ibm-not-wf-P12-ibm12n02',
     'ibm-not-wf-P12-ibm12n01','ibm-not-wf-P11-ibm11n04',
     'ibm-not-wf-P11-ibm11n03','ibm-not-wf-P11-ibm11n02',
     'ibm-not-wf-P11-ibm11n01','ibm-not-wf-P10-ibm10n08',
     'ibm-not-wf-P10-ibm10n07','ibm-not-wf-P10-ibm10n06',
     'ibm-not-wf-P10-ibm10n05','ibm-not-wf-P10-ibm10n04',
     'ibm-not-wf-P10-ibm10n03','ibm-not-wf-P10-ibm10n02',
     'ibm-not-wf-P10-ibm10n01','ibm-not-wf-P09-ibm09n04',
     'ibm-not-wf-P09-ibm09n03','ibm-not-wf-P09-ibm09n02',
     'ibm-not-wf-P09-ibm09n01','ibm-not-wf-P05-ibm05n03',
     'ibm-not-wf-P05-ibm05n02','ibm-not-wf-P05-ibm05n01',
     'ibm-not-wf-P04-ibm04n18','ibm-not-wf-P04-ibm04n17',
     'ibm-not-wf-P04-ibm04n16','ibm-not-wf-P04-ibm04n15',
     'ibm-not-wf-P04-ibm04n14','ibm-not-wf-P04-ibm04n13',
     'ibm-not-wf-P04-ibm04n12','ibm-not-wf-P04-ibm04n11',
     'ibm-not-wf-P04-ibm04n10','ibm-not-wf-P04-ibm04n09',
     'ibm-not-wf-P04-ibm04n08','ibm-not-wf-P04-ibm04n07',
     'ibm-not-wf-P04-ibm04n06','ibm-not-wf-P04-ibm04n05',
     'ibm-not-wf-P04-ibm04n04','ibm-not-wf-P04-ibm04n03',
     'ibm-not-wf-P04-ibm04n02','ibm-not-wf-P04-ibm04n01',
     'ibm-not-wf-P03-ibm03n01','ibm-not-wf-P02-ibm02n33',
     'ibm-not-wf-P02-ibm02n32','ibm-not-wf-P02-ibm02n31',
     'ibm-not-wf-P02-ibm02n30','ibm-not-wf-P02-ibm02n29',
     'ibm-not-wf-P02-ibm02n28','ibm-not-wf-P02-ibm02n27',
     'ibm-not-wf-P02-ibm02n26','ibm-not-wf-P02-ibm02n25',
     'ibm-not-wf-P02-ibm02n24','ibm-not-wf-P02-ibm02n23',
     'ibm-not-wf-P02-ibm02n22','ibm-not-wf-P02-ibm02n21',
     'ibm-not-wf-P02-ibm02n20','ibm-not-wf-P02-ibm02n19',
     'ibm-not-wf-P02-ibm02n18','ibm-not-wf-P02-ibm02n17',
     'ibm-not-wf-P02-ibm02n16','ibm-not-wf-P02-ibm02n15',
     'ibm-not-wf-P02-ibm02n14','ibm-not-wf-P02-ibm02n13',
     'ibm-not-wf-P02-ibm02n12','ibm-not-wf-P02-ibm02n11',
     'ibm-not-wf-P02-ibm02n10','ibm-not-wf-P02-ibm02n09',
     'ibm-not-wf-P02-ibm02n08','ibm-not-wf-P02-ibm02n07',
     'ibm-not-wf-P02-ibm02n06','ibm-not-wf-P02-ibm02n05',
     'ibm-not-wf-P02-ibm02n04','ibm-not-wf-P02-ibm02n03',
     'ibm-not-wf-P02-ibm02n02','ibm-not-wf-P02-ibm02n01',
     'ibm-not-wf-P01-ibm01n03','ibm-not-wf-P01-ibm01n02',
     'ibm-not-wf-P01-ibm01n01'].

'ibm-valid'(suite) -> 
    %% 149 test cases
    ['ibm-valid-P89-ibm89v01','ibm-valid-P88-ibm88v01',
     'ibm-valid-P87-ibm87v01','ibm-valid-P86-ibm86v01',
     'ibm-valid-P85-ibm85v01','ibm-valid-P82-ibm82v01',
     'ibm-valid-P79-ibm79v01','ibm-valid-P78-ibm78v01',
     'ibm-valid-P70-ibm70v01','ibm-valid-P69-ibm69v02',
     'ibm-valid-P69-ibm69v01','ibm-valid-P68-ibm68v02',
     'ibm-valid-P68-ibm68v01','ibm-valid-P67-ibm67v01',
     'ibm-valid-P66-ibm66v01','ibm-valid-P65-ibm65v02',
     'ibm-valid-P65-ibm65v01','ibm-valid-P64-ibm64v03',
     'ibm-valid-P64-ibm64v02','ibm-valid-P64-ibm64v01',
     'ibm-valid-P63-ibm63v05','ibm-valid-P63-ibm63v04',
     'ibm-valid-P63-ibm63v03','ibm-valid-P63-ibm63v02',
     'ibm-valid-P63-ibm63v01','ibm-valid-P62-ibm62v05',
     'ibm-valid-P62-ibm62v04','ibm-valid-P62-ibm62v03',
     'ibm-valid-P62-ibm62v02','ibm-valid-P62-ibm62v01',
     'ibm-valid-P61-ibm61v02','ibm-valid-P61-ibm61v01',
     'ibm-valid-P60-ibm60v04','ibm-valid-P60-ibm60v03',
     'ibm-valid-P60-ibm60v02','ibm-valid-P60-ibm60v01',
     'ibm-valid-P59-ibm59v02','ibm-valid-P59-ibm59v01',
     'ibm-valid-P58-ibm58v02','ibm-valid-P58-ibm58v01',
     'ibm-valid-P57-ibm57v01','ibm-valid-P56-ibm56v10',
     'ibm-valid-P56-ibm56v09','ibm-valid-P56-ibm56v08',
     'ibm-valid-P56-ibm56v07','ibm-valid-P56-ibm56v06',
     'ibm-valid-P56-ibm56v05','ibm-valid-P56-ibm56v04',
     'ibm-valid-P56-ibm56v03','ibm-valid-P56-ibm56v02',
     'ibm-valid-P56-ibm56v01','ibm-valid-P55-ibm55v01',
     'ibm-valid-P54-ibm54v03','ibm-valid-P54-ibm54v02',
     'ibm-valid-P54-ibm54v01','ibm-valid-P52-ibm52v01',
     'ibm-valid-P51-ibm51v02','ibm-valid-P51-ibm51v01',
     'ibm-valid-P50-ibm50v01','ibm-valid-P49-ibm49v01',
     'ibm-valid-P47-ibm47v01','ibm-valid-P45-ibm45v01',
     'ibm-valid-P44-ibm44v01','ibm-valid-P43-ibm43v01',
     'ibm-valid-P42-ibm42v01','ibm-valid-P41-ibm41v01',
     'ibm-valid-P40-ibm40v01','ibm-valid-P39-ibm39v01',
     'ibm-valid-P38-ibm38v01','ibm-valid-P37-ibm37v01',
     'ibm-valid-P36-ibm36v01','ibm-valid-P35-ibm35v01',
     'ibm-valid-P34-ibm34v01','ibm-valid-P33-ibm33v01',
     'ibm-valid-P32-ibm32v04','ibm-valid-P32-ibm32v03',
     'ibm-valid-P32-ibm32v02','ibm-valid-P32-ibm32v01',
     'ibm-valid-P31-ibm31v01','ibm-valid-P30-ibm30v02',
     'ibm-valid-P30-ibm30v01','ibm-valid-P29-ibm29v02',
     'ibm-valid-P29-ibm29v01','ibm-valid-P28-ibm28v02',
     'ibm-valid-P28-ibm28v01','ibm-valid-P27-ibm27v03',
     'ibm-valid-P27-ibm27v02','ibm-valid-P27-ibm27v01',
     'ibm-valid-P26-ibm26v01','ibm-valid-P25-ibm25v04',
     'ibm-valid-P25-ibm25v03','ibm-valid-P25-ibm25v02',
     'ibm-valid-P25-ibm25v01','ibm-valid-P24-ibm24v02',
     'ibm-valid-P24-ibm24v01','ibm-valid-P23-ibm23v06',
     'ibm-valid-P23-ibm23v05','ibm-valid-P23-ibm23v04',
     'ibm-valid-P23-ibm23v03','ibm-valid-P23-ibm23v02',
     'ibm-valid-P23-ibm23v01','ibm-valid-P22-ibm22v07',
     'ibm-valid-P22-ibm22v06','ibm-valid-P22-ibm22v05',
     'ibm-valid-P22-ibm22v04','ibm-valid-P22-ibm22v03',
     'ibm-valid-P22-ibm22v02','ibm-valid-P22-ibm22v01',
     'ibm-valid-P21-ibm21v01','ibm-valid-P20-ibm20v02',
     'ibm-valid-P20-ibm20v01','ibm-valid-P19-ibm19v01',
     'ibm-valid-P18-ibm18v01','ibm-valid-P17-ibm17v01',
     'ibm-valid-P16-ibm16v03','ibm-valid-P16-ibm16v02',
     'ibm-valid-P16-ibm16v01','ibm-valid-P15-ibm15v04',
     'ibm-valid-P15-ibm15v03','ibm-valid-P15-ibm15v02',
     'ibm-valid-P15-ibm15v01','ibm-valid-P14-ibm14v03',
     'ibm-valid-P14-ibm14v02','ibm-valid-P14-ibm14v01',
     'ibm-valid-P13-ibm13v01','ibm-valid-P12-ibm12v04',
     'ibm-valid-P12-ibm12v03','ibm-valid-P12-ibm12v02',
     'ibm-valid-P12-ibm12v01','ibm-valid-P11-ibm11v04',
     'ibm-valid-P11-ibm11v03','ibm-valid-P11-ibm11v02',
     'ibm-valid-P11-ibm11v01','ibm-valid-P10-ibm10v08',
     'ibm-valid-P10-ibm10v07','ibm-valid-P10-ibm10v06',
     'ibm-valid-P10-ibm10v05','ibm-valid-P10-ibm10v04',
     'ibm-valid-P10-ibm10v03','ibm-valid-P10-ibm10v02',
     'ibm-valid-P10-ibm10v01','ibm-valid-P09-ibm09v05',
     'ibm-valid-P09-ibm09v04','ibm-valid-P09-ibm09v03',
     'ibm-valid-P09-ibm09v02','ibm-valid-P09-ibm09v01',
     'ibm-valid-P03-ibm03v01','ibm-valid-P02-ibm02v01',
     'ibm-valid-P01-ibm01v01'].

%%----------------------------------------------------------------------
%% Initializations
%%----------------------------------------------------------------------

init_per_suite(doc) ->
    ["Starts the test suite"];
init_per_suite(Config) ->
    Dog=test_server:timetrap({minutes,10}),
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line ok=erl_tar:extract("ibm.tgz",[compressed]),
    ?line ok=erl_tar:extract("japanese.tgz",[compressed]),
    ?line ok=erl_tar:extract("oasis.tgz",[compressed]),
    ?line ok=erl_tar:extract("sun.tgz",[compressed]),
    ?line ok=erl_tar:extract("xmltest.tgz",[compressed]),
    ?line ok = change_mode(["ibm","japanese","oasis",
			    "sun","xmltest"]),
    [{watchdog, Dog}|Config].


-ifndef(dont_rm_test_dirs).
end_per_suite(doc) ->
    ["Stops the test suite"];
end_per_suite(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line ok=rm_files(["ibm","japanese","oasis","sun","xmltest"]),
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    lists:keydelete(watchdog,1,Config).

-else.
end_per_suite(doc) ->
    ["Stops the test suite"];
end_per_suite(Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    lists:keydelete(watchdog,1,Config).
-endif.

%% initialization before each testcase
init_per_testcase(_TestCase,Config) ->
    io:format("Config:~n~p",[Config]),
    ?line {ok, _} = file:read_file_info(filename:join([?config(priv_dir,Config)])),
    ?line code:add_patha(?config(priv_dir,Config)),
    Dog=test_server:timetrap({minutes,10}),
    [{watchdog, Dog}|Config].


%% clean up after each testcase
end_per_testcase(_Func,Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.


%%----------------------------------------------------------------------
%% Test cases
%%----------------------------------------------------------------------
'v-pe02'(suite) -> [];
'v-pe02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"v-pe02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'v-pe03'(suite) -> [];
'v-pe03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"v-pe03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'v-pe00'(suite) -> [];
'v-pe00'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"v-pe00.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'v-lang06'(suite) -> [];
'v-lang06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"v-lang06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'v-lang05'(suite) -> [];
'v-lang05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"v-lang05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'v-lang04'(suite) -> [];
'v-lang04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"v-lang04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'v-lang03'(suite) -> [];
'v-lang03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"v-lang03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'v-lang02'(suite) -> [];
'v-lang02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"v-lang02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'v-lang01'(suite) -> [];
'v-lang01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"v-lang01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'v-sgml01'(suite) -> [];
'v-sgml01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"v-sgml01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sa05'(suite) -> [];
'sa05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sa05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sa04'(suite) -> [];
'sa04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sa04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sa03'(suite) -> [];
'sa03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sa03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sa02'(suite) -> [];
'sa02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sa02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sa01'(suite) -> [];
'sa01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sa01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'required00'(suite) -> [];
'required00'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"required00.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional'(suite) -> [];
'optional'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'notation01'(suite) -> [];
'notation01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"notation01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-sa04'(suite) -> [];
'not-sa04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"not-sa04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-sa03'(suite) -> [];
'not-sa03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"not-sa03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-sa02'(suite) -> [];
'not-sa02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"not-sa02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-sa01'(suite) -> [];
'not-sa01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"not-sa01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ext02'(suite) -> [];
'ext02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"ext02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ext01'(suite) -> [];
'ext01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"ext01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'element'(suite) -> [];
'element'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"element.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'dtd01'(suite) -> [];
'dtd01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"dtd01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'dtd00'(suite) -> [];
'dtd00'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"dtd00.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'pe01'(suite) -> [];
'pe01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"pe01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

%%----------------------------------------------------------------------

'empty'(suite) -> [];
'empty'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"empty.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'utf16l'(suite) -> [];
'utf16l'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"utf16l.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'utf16b'(suite) -> [];
'utf16b'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"utf16b.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr16'(suite) -> [];
'attr16'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr16.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr15'(suite) -> [];
'attr15'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr15.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr14'(suite) -> [];
'attr14'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr14.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr13'(suite) -> [];
'attr13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr12'(suite) -> [];
'attr12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr11'(suite) -> [];
'attr11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr10'(suite) -> [];
'attr10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr09'(suite) -> [];
'attr09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr08'(suite) -> [];
'attr08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr07'(suite) -> [];
'attr07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr06'(suite) -> [];
'attr06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr05'(suite) -> [];
'attr05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr04'(suite) -> [];
'attr04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr03'(suite) -> [];
'attr03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr02'(suite) -> [];
'attr02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attr01'(suite) -> [];
'attr01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attr01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'root'(suite) -> [];
'root'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"root.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-required02'(suite) -> [];
'inv-required02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-required02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-required01'(suite) -> [];
'inv-required01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-required01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-required00'(suite) -> [];
'inv-required00'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-required00.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional25'(suite) -> [];
'optional25'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional25.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional24'(suite) -> [];
'optional24'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional24.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional23'(suite) -> [];
'optional23'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional23.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional22'(suite) -> [];
'optional22'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional22.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional21'(suite) -> [];
'optional21'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional21.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional20'(suite) -> [];
'optional20'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional20.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional14'(suite) -> [];
'optional14'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional14.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional13'(suite) -> [];
'optional13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional12'(suite) -> [];
'optional12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional11'(suite) -> [];
'optional11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional10'(suite) -> [];
'optional10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional09'(suite) -> [];
'optional09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional08'(suite) -> [];
'optional08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional07'(suite) -> [];
'optional07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional06'(suite) -> [];
'optional06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional05'(suite) -> [];
'optional05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional04'(suite) -> [];
'optional04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional03'(suite) -> [];
'optional03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional02'(suite) -> [];
'optional02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'optional01'(suite) -> [];
'optional01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"optional01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-not-sa14'(suite) -> [];
'inv-not-sa14'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-not-sa14.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-not-sa13'(suite) -> [];
'inv-not-sa13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-not-sa13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-not-sa12'(suite) -> [];
'inv-not-sa12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-not-sa12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-not-sa11'(suite) -> [];
'inv-not-sa11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-not-sa11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-not-sa10'(suite) -> [];
'inv-not-sa10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-not-sa10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-not-sa09'(suite) -> [];
'inv-not-sa09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-not-sa09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-not-sa08'(suite) -> [];
'inv-not-sa08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-not-sa08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-not-sa07'(suite) -> [];
'inv-not-sa07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-not-sa07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-not-sa06'(suite) -> [];
'inv-not-sa06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-not-sa06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-not-sa05'(suite) -> [];
'inv-not-sa05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-not-sa05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-not-sa04'(suite) -> [];
'inv-not-sa04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-not-sa04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-not-sa02'(suite) -> [];
'inv-not-sa02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-not-sa02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-not-sa01'(suite) -> [];
'inv-not-sa01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-not-sa01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'id09'(suite) -> [];
'id09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"id09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'id08'(suite) -> [];
'id08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"id08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'id07'(suite) -> [];
'id07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"id07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'id06'(suite) -> [];
'id06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"id06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'id05'(suite) -> [];
'id05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"id05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'id04'(suite) -> [];
'id04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"id04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'id03'(suite) -> [];
'id03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"id03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'id02'(suite) -> [];
'id02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"id02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'id01'(suite) -> [];
'id01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"id01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'el06'(suite) -> [];
'el06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"el06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'el05'(suite) -> [];
'el05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"el05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'el04'(suite) -> [];
'el04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"el04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'el03'(suite) -> [];
'el03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"el03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'el02'(suite) -> [];
'el02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"el02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'el01'(suite) -> [];
'el01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"el01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-dtd03'(suite) -> [];
'inv-dtd03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-dtd03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-dtd02'(suite) -> [];
'inv-dtd02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-dtd02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'inv-dtd01'(suite) -> [];
'inv-dtd01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"inv-dtd01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

%%----------------------------------------------------------------------

'sgml13'(suite) -> [];
'sgml13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sgml13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sgml12'(suite) -> [];
'sgml12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sgml12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sgml11'(suite) -> [];
'sgml11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sgml11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sgml10'(suite) -> [];
'sgml10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sgml10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sgml09'(suite) -> [];
'sgml09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sgml09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sgml08'(suite) -> [];
'sgml08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sgml08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sgml07'(suite) -> [];
'sgml07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sgml07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sgml06'(suite) -> [];
'sgml06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sgml06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sgml05'(suite) -> [];
'sgml05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sgml05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sgml04'(suite) -> [];
'sgml04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sgml04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sgml03'(suite) -> [];
'sgml03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sgml03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sgml02'(suite) -> [];
'sgml02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sgml02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'sgml01'(suite) -> [];
'sgml01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"sgml01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'pubid05'(suite) -> [];
'pubid05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"pubid05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'pubid04'(suite) -> [];
'pubid04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"pubid04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'pubid03'(suite) -> [];
'pubid03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"pubid03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'pubid02'(suite) -> [];
'pubid02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"pubid02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'pubid01'(suite) -> [];
'pubid01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"pubid01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'pi'(suite) -> [];
'pi'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"pi.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'encoding07'(suite) -> [];
'encoding07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"encoding07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'encoding06'(suite) -> [];
'encoding06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"encoding06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'encoding05'(suite) -> [];
'encoding05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"encoding05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'encoding04'(suite) -> [];
'encoding04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"encoding04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'encoding03'(suite) -> [];
'encoding03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"encoding03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'encoding02'(suite) -> [];
'encoding02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"encoding02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'encoding01'(suite) -> [];
'encoding01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"encoding01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'element04'(suite) -> [];
'element04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"element04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'element03'(suite) -> [];
'element03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"element03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'element02'(suite) -> [];
'element02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"element02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'element01'(suite) -> [];
'element01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"element01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'element00'(suite) -> [];
'element00'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"element00.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'dtd07'(suite) -> [];
'dtd07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"dtd07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'dtd05'(suite) -> [];
'dtd05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"dtd05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'dtd04'(suite) -> [];
'dtd04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"dtd04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'dtd03'(suite) -> [];
'dtd03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"dtd03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'dtd02'(suite) -> [];
'dtd02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"dtd02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'nwf-dtd01'(suite) -> [];
'nwf-dtd01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"nwf-dtd01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'nwf-dtd00'(suite) -> [];
'nwf-dtd00'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"nwf-dtd00.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'decl01'(suite) -> [];
'decl01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"decl01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'content03'(suite) -> [];
'content03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"content03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'content02'(suite) -> [];
'content02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"content02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'content01'(suite) -> [];
'content01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"content01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'cond02'(suite) -> [];
'cond02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"cond02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'cond01'(suite) -> [];
'cond01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"cond01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attlist11'(suite) -> [];
'attlist11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attlist11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attlist10'(suite) -> [];
'attlist10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attlist10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attlist09'(suite) -> [];
'attlist09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attlist09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attlist08'(suite) -> [];
'attlist08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attlist08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attlist07'(suite) -> [];
'attlist07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attlist07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attlist06'(suite) -> [];
'attlist06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attlist06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attlist05'(suite) -> [];
'attlist05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attlist05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attlist04'(suite) -> [];
'attlist04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attlist04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attlist03'(suite) -> [];
'attlist03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attlist03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attlist02'(suite) -> [];
'attlist02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attlist02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'attlist01'(suite) -> [];
'attlist01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"attlist01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa03'(suite) -> [];
'not-wf-sa03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"not-wf-sa03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

%%----------------------------------------------------------------------

'uri01'(suite) -> [];
'uri01'(doc) ->
    ["URI fragments disallowed"];
'uri01'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),sun,"uri01.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["URI fragments disallowed"]}.

%%----------------------------------------------------------------------

'valid-ext-sa-014'(suite) -> [];
'valid-ext-sa-014'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-ext-sa-014.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-ext-sa-013'(suite) -> [];
'valid-ext-sa-013'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-ext-sa-013.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-ext-sa-012'(suite) -> [];
'valid-ext-sa-012'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-ext-sa-012.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-ext-sa-011'(suite) -> [];
'valid-ext-sa-011'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-ext-sa-011.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-ext-sa-009'(suite) -> [];
'valid-ext-sa-009'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-ext-sa-009.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-ext-sa-008'(suite) -> [];
'valid-ext-sa-008'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-ext-sa-008.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-ext-sa-007'(suite) -> [];
'valid-ext-sa-007'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-ext-sa-007.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-ext-sa-006'(suite) -> [];
'valid-ext-sa-006'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-ext-sa-006.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-ext-sa-005'(suite) -> [];
'valid-ext-sa-005'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-ext-sa-005.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-ext-sa-004'(suite) -> [];
'valid-ext-sa-004'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-ext-sa-004.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-ext-sa-003'(suite) -> [];
'valid-ext-sa-003'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-ext-sa-003.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-ext-sa-002'(suite) -> [];
'valid-ext-sa-002'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-ext-sa-002.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-ext-sa-001'(suite) -> [];
'valid-ext-sa-001'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-ext-sa-001.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-031'(suite) -> [];
'valid-not-sa-031'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-031.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-030'(suite) -> [];
'valid-not-sa-030'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-030.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-029'(suite) -> [];
'valid-not-sa-029'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-029.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-028'(suite) -> [];
'valid-not-sa-028'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-028.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-027'(suite) -> [];
'valid-not-sa-027'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-027.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-026'(suite) -> [];
'valid-not-sa-026'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-026.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-025'(suite) -> [];
'valid-not-sa-025'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-025.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-024'(suite) -> [];
'valid-not-sa-024'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-024.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-023'(suite) -> [];
'valid-not-sa-023'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-023.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-021'(suite) -> [];
'valid-not-sa-021'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-021.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-020'(suite) -> [];
'valid-not-sa-020'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-020.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-019'(suite) -> [];
'valid-not-sa-019'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-019.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-018'(suite) -> [];
'valid-not-sa-018'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-018.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-017'(suite) -> [];
'valid-not-sa-017'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-017.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-016'(suite) -> [];
'valid-not-sa-016'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-016.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-015'(suite) -> [];
'valid-not-sa-015'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-015.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-014'(suite) -> [];
'valid-not-sa-014'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-014.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-013'(suite) -> [];
'valid-not-sa-013'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-013.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-012'(suite) -> [];
'valid-not-sa-012'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-012.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-011'(suite) -> [];
'valid-not-sa-011'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-011.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-010'(suite) -> [];
'valid-not-sa-010'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-010.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-009'(suite) -> [];
'valid-not-sa-009'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-009.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-008'(suite) -> [];
'valid-not-sa-008'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-008.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-007'(suite) -> [];
'valid-not-sa-007'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-007.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-006'(suite) -> [];
'valid-not-sa-006'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-006.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-005'(suite) -> [];
'valid-not-sa-005'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-005.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-004'(suite) -> [];
'valid-not-sa-004'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-004.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-003'(suite) -> [];
'valid-not-sa-003'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-003.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-002'(suite) -> [];
'valid-not-sa-002'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-002.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-not-sa-001'(suite) -> [];
'valid-not-sa-001'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-not-sa-001.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-119'(suite) -> [];
'valid-sa-119'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-119.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-118'(suite) -> [];
'valid-sa-118'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-118.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-117'(suite) -> [];
'valid-sa-117'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-117.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-116'(suite) -> [];
'valid-sa-116'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-116.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-115'(suite) -> [];
'valid-sa-115'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-115.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-114'(suite) -> [];
'valid-sa-114'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-114.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-113'(suite) -> [];
'valid-sa-113'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-113.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-112'(suite) -> [];
'valid-sa-112'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-112.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-111'(suite) -> [];
'valid-sa-111'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-111.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-110'(suite) -> [];
'valid-sa-110'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-110.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-109'(suite) -> [];
'valid-sa-109'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-109.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-108'(suite) -> [];
'valid-sa-108'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-108.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-107'(suite) -> [];
'valid-sa-107'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-107.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-106'(suite) -> [];
'valid-sa-106'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-106.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-105'(suite) -> [];
'valid-sa-105'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-105.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-104'(suite) -> [];
'valid-sa-104'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-104.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-103'(suite) -> [];
'valid-sa-103'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-103.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-102'(suite) -> [];
'valid-sa-102'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-102.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-101'(suite) -> [];
'valid-sa-101'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-101.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-100'(suite) -> [];
'valid-sa-100'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
%  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-100.xml"]),[]),
%  ?line xmerl:export([A],xmerl_test).
    {skip,["recursive xml spec"]}.

'valid-sa-099'(suite) -> [];
'valid-sa-099'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-099.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-098'(suite) -> [];
'valid-sa-098'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-098.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-097'(suite) -> [];
'valid-sa-097'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-097.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-096'(suite) -> [];
'valid-sa-096'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-096.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-095'(suite) -> [];
'valid-sa-095'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-095.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-094'(suite) -> [];
'valid-sa-094'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-094.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-093'(suite) -> [];
'valid-sa-093'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-093.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-092'(suite) -> [];
'valid-sa-092'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-092.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-091'(suite) -> [];
'valid-sa-091'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-091.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-090'(suite) -> [];
'valid-sa-090'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-090.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-089'(suite) -> [];
'valid-sa-089'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-089.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-088'(suite) -> [];
'valid-sa-088'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-088.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-087'(suite) -> [];
'valid-sa-087'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-087.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-086'(suite) -> [];
'valid-sa-086'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-086.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-085'(suite) -> [];
'valid-sa-085'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-085.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-084'(suite) -> [];
'valid-sa-084'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-084.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-083'(suite) -> [];
'valid-sa-083'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-083.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-082'(suite) -> [];
'valid-sa-082'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-082.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-081'(suite) -> [];
'valid-sa-081'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-081.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-080'(suite) -> [];
'valid-sa-080'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-080.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-079'(suite) -> [];
'valid-sa-079'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-079.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-078'(suite) -> [];
'valid-sa-078'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-078.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-077'(suite) -> [];
'valid-sa-077'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-077.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-076'(suite) -> [];
'valid-sa-076'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-076.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-075'(suite) -> [];
'valid-sa-075'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-075.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-074'(suite) -> [];
'valid-sa-074'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-074.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-073'(suite) -> [];
'valid-sa-073'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-073.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-072'(suite) -> [];
'valid-sa-072'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-072.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-071'(suite) -> [];
'valid-sa-071'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-071.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-070'(suite) -> [];
'valid-sa-070'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-070.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-069'(suite) -> [];
'valid-sa-069'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-069.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-068'(suite) -> [];
'valid-sa-068'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-068.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-067'(suite) -> [];
'valid-sa-067'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-067.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-066'(suite) -> [];
'valid-sa-066'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-066.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-065'(suite) -> [];
'valid-sa-065'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-065.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-064'(suite) -> [];
'valid-sa-064'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-064.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Fails to handle UTF-8 encoded names, when they are converted to atoms"]}.

'valid-sa-063'(suite) -> [];
'valid-sa-063'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-063.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Fails to handle Unicode integer (UTF-8) encoded names, when they are converted to atoms"]}.

'valid-sa-062'(suite) -> [];
'valid-sa-062'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-062.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-061'(suite) -> [];
'valid-sa-061'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-061.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-060'(suite) -> [];
'valid-sa-060'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-060.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-059'(suite) -> [];
'valid-sa-059'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-059.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-058'(suite) -> [];
'valid-sa-058'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-058.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-057'(suite) -> [];
'valid-sa-057'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-057.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-056'(suite) -> [];
'valid-sa-056'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-056.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-055'(suite) -> [];
'valid-sa-055'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-055.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-054'(suite) -> [];
'valid-sa-054'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-054.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-053'(suite) -> [];
'valid-sa-053'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-053.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-052'(suite) -> [];
'valid-sa-052'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-052.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-051'(suite) -> [];
'valid-sa-051'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-051.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Fails to handle Unicode integer (UTF-16) encoded names, when they are converted to atoms"]}.
'valid-sa-050'(suite) -> [];
'valid-sa-050'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-050.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-049'(suite) -> [];
'valid-sa-049'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-049.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-048'(suite) -> [];
'valid-sa-048'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-048.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-047'(suite) -> [];
'valid-sa-047'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-047.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-046'(suite) -> [];
'valid-sa-046'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-046.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-045'(suite) -> [];
'valid-sa-045'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-045.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-044'(suite) -> [];
'valid-sa-044'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-044.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-043'(suite) -> [];
'valid-sa-043'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-043.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-042'(suite) -> [];
'valid-sa-042'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-042.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-041'(suite) -> [];
'valid-sa-041'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-041.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-040'(suite) -> [];
'valid-sa-040'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-040.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-039'(suite) -> [];
'valid-sa-039'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-039.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-038'(suite) -> [];
'valid-sa-038'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-038.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-037'(suite) -> [];
'valid-sa-037'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-037.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-036'(suite) -> [];
'valid-sa-036'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-036.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-035'(suite) -> [];
'valid-sa-035'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-035.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-034'(suite) -> [];
'valid-sa-034'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-034.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-033'(suite) -> [];
'valid-sa-033'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-033.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-032'(suite) -> [];
'valid-sa-032'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-032.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-031'(suite) -> [];
'valid-sa-031'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-031.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-030'(suite) -> [];
'valid-sa-030'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-030.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-029'(suite) -> [];
'valid-sa-029'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-029.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-028'(suite) -> [];
'valid-sa-028'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-028.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-027'(suite) -> [];
'valid-sa-027'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-027.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-026'(suite) -> [];
'valid-sa-026'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-026.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-025'(suite) -> [];
'valid-sa-025'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-025.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-024'(suite) -> [];
'valid-sa-024'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-024.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-023'(suite) -> [];
'valid-sa-023'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-023.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-022'(suite) -> [];
'valid-sa-022'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-022.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-021'(suite) -> [];
'valid-sa-021'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-021.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-020'(suite) -> [];
'valid-sa-020'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-020.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-019'(suite) -> [];
'valid-sa-019'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-019.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-018'(suite) -> [];
'valid-sa-018'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-018.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-017'(suite) -> [];
'valid-sa-017'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-017.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-016'(suite) -> [];
'valid-sa-016'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-016.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-015'(suite) -> [];
'valid-sa-015'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-015.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-014'(suite) -> [];
'valid-sa-014'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-014.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-013'(suite) -> [];
'valid-sa-013'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-013.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-012'(suite) -> [];
'valid-sa-012'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-012.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-011'(suite) -> [];
'valid-sa-011'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-011.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-010'(suite) -> [];
'valid-sa-010'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-010.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-009'(suite) -> [];
'valid-sa-009'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-009.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-008'(suite) -> [];
'valid-sa-008'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-008.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-007'(suite) -> [];
'valid-sa-007'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-007.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-006'(suite) -> [];
'valid-sa-006'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-006.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-005'(suite) -> [];
'valid-sa-005'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-005.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-004'(suite) -> [];
'valid-sa-004'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-004.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-003'(suite) -> [];
'valid-sa-003'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-003.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-002'(suite) -> [];
'valid-sa-002'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-002.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'valid-sa-001'(suite) -> [];
'valid-sa-001'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"valid-sa-001.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'invalid-not-sa-022'(suite) -> [];
'invalid-not-sa-022'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"invalid-not-sa-022.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'invalid--006'(suite) -> [];
'invalid--006'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"invalid--006.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'invalid--005'(suite) -> [];
'invalid--005'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"invalid--005.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'invalid--002'(suite) -> [];
'invalid--002'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"invalid--002.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-ext-sa-003'(suite) -> [];
'not-wf-ext-sa-003'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-ext-sa-003.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-ext-sa-002'(suite) -> [];
'not-wf-ext-sa-002'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-ext-sa-002.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-ext-sa-001'(suite) -> [];
'not-wf-ext-sa-001'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-ext-sa-001.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-not-sa-009'(suite) -> [];
'not-wf-not-sa-009'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-not-sa-009.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-not-sa-008'(suite) -> [];
'not-wf-not-sa-008'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-not-sa-008.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-not-sa-007'(suite) -> [];
'not-wf-not-sa-007'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-not-sa-007.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-not-sa-006'(suite) -> [];
'not-wf-not-sa-006'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-not-sa-006.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-not-sa-005'(suite) -> [];
'not-wf-not-sa-005'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-not-sa-005.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-not-sa-004'(suite) -> [];
'not-wf-not-sa-004'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-not-sa-004.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-not-sa-003'(suite) -> [];
'not-wf-not-sa-003'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-not-sa-003.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-not-sa-002'(suite) -> [];
'not-wf-not-sa-002'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-not-sa-002.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-not-sa-001'(suite) -> [];
'not-wf-not-sa-001'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-not-sa-001.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-186'(suite) -> [];
'not-wf-sa-186'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-186.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-185'(suite) -> [];
'not-wf-sa-185'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-185.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-184'(suite) -> [];
'not-wf-sa-184'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-184.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-183'(suite) -> [];
'not-wf-sa-183'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-183.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-182'(suite) -> [];
'not-wf-sa-182'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-182.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-181'(suite) -> [];
'not-wf-sa-181'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-181.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-180'(suite) -> [];
'not-wf-sa-180'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-180.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-179'(suite) -> [];
'not-wf-sa-179'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-179.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-178'(suite) -> [];
'not-wf-sa-178'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-178.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-177'(suite) -> [];
'not-wf-sa-177'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-177.xml"]),[]),
%  ?line xmerl:export([A],xmerl_test).
    {skip,["do not support UTF-8 (only Latin-1), therefore not ","able to check the illegal FFFF/FFFE (Unicode) characters"]}.

'not-wf-sa-176'(suite) -> [];
'not-wf-sa-176'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-176.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-175'(suite) -> [];
'not-wf-sa-175'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-175.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["do not support UTF-8 (only Latin-1), therefore not ","able to check the illegal FFFF/FFFE (Unicode) characters"]}.

'not-wf-sa-174'(suite) -> [];
'not-wf-sa-174'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-174.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["do not support UTF-8 (only Latin-1), therefore not ","able to check the illegal FFFF/FFFE (Unicode) characters"]}.

'not-wf-sa-173'(suite) -> [];
'not-wf-sa-173'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-173.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["do not support UTF-8 (only Latin-1), therefore not ","able to check the illegal FFFF/FFFE (Unicode) characters"]}.

'not-wf-sa-172'(suite) -> [];
'not-wf-sa-172'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-172.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["do not support UTF-8 (only Latin-1), therefore not ","able to check the illegal FFFF/FFFE (Unicode) characters"]}.

'not-wf-sa-171'(suite) -> [];
'not-wf-sa-171'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-171.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["do not support UTF-8 (only Latin-1), therefore not ","able to check the illegal FFFF/FFFE (Unicode) characters"]}.

'not-wf-sa-170'(suite) -> [];
'not-wf-sa-170'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-170.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["UTF-8 encoding of UCS-4 characters"]}.

'not-wf-sa-169'(suite) -> [];
'not-wf-sa-169'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-169.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["UTF-8 encoding of an illegal unpaired surrogate (DC00)"]}.

'not-wf-sa-168'(suite) -> [];
'not-wf-sa-168'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-168.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["UTF-8 encoding of an illegal unpaired surrogate (D800)"]}.

'not-wf-sa-167'(suite) -> [];
'not-wf-sa-167'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-167.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["UTF-8 encoding of an illegal FFFE"]}.

'not-wf-sa-166'(suite) -> [];
'not-wf-sa-166'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-166.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["UTF-8 encoding of an illegal FFFE"]}.

'not-wf-sa-165'(suite) -> [];
'not-wf-sa-165'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-165.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-164'(suite) -> [];
'not-wf-sa-164'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-164.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-163'(suite) -> [];
'not-wf-sa-163'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-163.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-162'(suite) -> [];
'not-wf-sa-162'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-162.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-161'(suite) -> [];
'not-wf-sa-161'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-161.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-160'(suite) -> [];
'not-wf-sa-160'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-160.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-159'(suite) -> [];
'not-wf-sa-159'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-159.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-158'(suite) -> [];
'not-wf-sa-158'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-158.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-157'(suite) -> [];
'not-wf-sa-157'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-157.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-156'(suite) -> [];
'not-wf-sa-156'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-156.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-155'(suite) -> [];
'not-wf-sa-155'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-155.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-154'(suite) -> [];
'not-wf-sa-154'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-154.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-153'(suite) -> [];
'not-wf-sa-153'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-153.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-152'(suite) -> [];
'not-wf-sa-152'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-152.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-151'(suite) -> [];
'not-wf-sa-151'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-151.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["don't bother wath's in the Misc production"]}.

'not-wf-sa-150'(suite) -> [];
'not-wf-sa-150'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-150.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-149'(suite) -> [];
'not-wf-sa-149'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-149.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-148'(suite) -> [];
'not-wf-sa-148'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-148.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-147'(suite) -> [];
'not-wf-sa-147'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-147.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-146'(suite) -> [];
'not-wf-sa-146'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-146.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-145'(suite) -> [];
'not-wf-sa-145'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-145.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-144'(suite) -> [];
'not-wf-sa-144'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-144.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-143'(suite) -> [];
'not-wf-sa-143'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-143.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-142'(suite) -> [];
'not-wf-sa-142'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-142.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-141'(suite) -> [];
'not-wf-sa-141'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-141.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-140'(suite) -> [];
'not-wf-sa-140'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-140.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-139'(suite) -> [];
'not-wf-sa-139'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-139.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-138'(suite) -> [];
'not-wf-sa-138'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-138.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-137'(suite) -> [];
'not-wf-sa-137'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-137.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-136'(suite) -> [];
'not-wf-sa-136'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-136.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-135'(suite) -> [];
'not-wf-sa-135'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-135.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-134'(suite) -> [];
'not-wf-sa-134'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-134.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-133'(suite) -> [];
'not-wf-sa-133'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-133.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-132'(suite) -> [];
'not-wf-sa-132'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-132.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-131'(suite) -> [];
'not-wf-sa-131'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-131.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-130'(suite) -> [];
'not-wf-sa-130'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-130.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-129'(suite) -> [];
'not-wf-sa-129'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-129.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-128'(suite) -> [];
'not-wf-sa-128'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-128.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-127'(suite) -> [];
'not-wf-sa-127'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-127.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-126'(suite) -> [];
'not-wf-sa-126'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-126.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-125'(suite) -> [];
'not-wf-sa-125'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-125.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-124'(suite) -> [];
'not-wf-sa-124'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-124.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-123'(suite) -> [];
'not-wf-sa-123'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-123.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-122'(suite) -> [];
'not-wf-sa-122'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-122.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-121'(suite) -> [];
'not-wf-sa-121'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-121.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-120'(suite) -> [];
'not-wf-sa-120'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-120.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-119'(suite) -> [];
'not-wf-sa-119'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-119.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-118'(suite) -> [];
'not-wf-sa-118'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-118.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-117'(suite) -> [];
'not-wf-sa-117'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-117.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-116'(suite) -> [];
'not-wf-sa-116'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-116.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-115'(suite) -> [];
'not-wf-sa-115'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-115.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-114'(suite) -> [];
'not-wf-sa-114'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-114.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-113'(suite) -> [];
'not-wf-sa-113'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-113.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-112'(suite) -> [];
'not-wf-sa-112'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-112.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-111'(suite) -> [];
'not-wf-sa-111'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-111.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-110'(suite) -> [];
'not-wf-sa-110'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-110.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-109'(suite) -> [];
'not-wf-sa-109'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-109.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-108'(suite) -> [];
'not-wf-sa-108'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-108.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-107'(suite) -> [];
'not-wf-sa-107'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-107.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-106'(suite) -> [];
'not-wf-sa-106'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-106.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-105'(suite) -> [];
'not-wf-sa-105'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-105.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-104'(suite) -> [];
'not-wf-sa-104'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-104.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-103'(suite) -> [];
'not-wf-sa-103'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-103.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-102'(suite) -> [];
'not-wf-sa-102'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-102.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-101'(suite) -> [];
'not-wf-sa-101'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-101.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-100'(suite) -> [];
'not-wf-sa-100'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-100.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-099'(suite) -> [];
'not-wf-sa-099'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-099.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-098'(suite) -> [];
'not-wf-sa-098'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-098.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-097'(suite) -> [];
'not-wf-sa-097'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-097.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-096'(suite) -> [];
'not-wf-sa-096'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-096.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-095'(suite) -> [];
'not-wf-sa-095'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-095.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-094'(suite) -> [];
'not-wf-sa-094'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-094.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-093'(suite) -> [];
'not-wf-sa-093'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-093.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-092'(suite) -> [];
'not-wf-sa-092'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-092.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-091'(suite) -> [];
'not-wf-sa-091'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-091.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-090'(suite) -> [];
'not-wf-sa-090'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-090.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-089'(suite) -> [];
'not-wf-sa-089'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-089.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-088'(suite) -> [];
'not-wf-sa-088'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-088.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-087'(suite) -> [];
'not-wf-sa-087'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-087.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-086'(suite) -> [];
'not-wf-sa-086'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-086.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-085'(suite) -> [];
'not-wf-sa-085'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-085.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-084'(suite) -> [];
'not-wf-sa-084'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-084.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-083'(suite) -> [];
'not-wf-sa-083'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-083.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-082'(suite) -> [];
'not-wf-sa-082'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-082.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-081'(suite) -> [];
'not-wf-sa-081'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-081.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-080'(suite) -> [];
'not-wf-sa-080'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-080.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-079'(suite) -> [];
'not-wf-sa-079'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-079.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-078'(suite) -> [];
'not-wf-sa-078'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-078.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-077'(suite) -> [];
'not-wf-sa-077'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-077.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-076'(suite) -> [];
'not-wf-sa-076'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-076.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-075'(suite) -> [];
'not-wf-sa-075'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-075.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-074'(suite) -> [];
'not-wf-sa-074'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-074.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-073'(suite) -> [];
'not-wf-sa-073'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-073.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-072'(suite) -> [];
'not-wf-sa-072'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-072.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-071'(suite) -> [];
'not-wf-sa-071'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-071.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-070'(suite) -> [];
'not-wf-sa-070'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-070.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-069'(suite) -> [];
'not-wf-sa-069'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-069.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-068'(suite) -> [];
'not-wf-sa-068'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-068.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-067'(suite) -> [];
'not-wf-sa-067'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-067.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-066'(suite) -> [];
'not-wf-sa-066'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-066.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-065'(suite) -> [];
'not-wf-sa-065'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-065.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-064'(suite) -> [];
'not-wf-sa-064'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-064.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-063'(suite) -> [];
'not-wf-sa-063'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-063.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-062'(suite) -> [];
'not-wf-sa-062'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-062.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-061'(suite) -> [];
'not-wf-sa-061'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-061.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-060'(suite) -> [];
'not-wf-sa-060'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-060.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-059'(suite) -> [];
'not-wf-sa-059'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-059.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-058'(suite) -> [];
'not-wf-sa-058'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-058.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-057'(suite) -> [];
'not-wf-sa-057'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-057.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-056'(suite) -> [];
'not-wf-sa-056'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-056.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-055'(suite) -> [];
'not-wf-sa-055'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-055.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-054'(suite) -> [];
'not-wf-sa-054'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-054.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-053'(suite) -> [];
'not-wf-sa-053'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-053.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-052'(suite) -> [];
'not-wf-sa-052'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-052.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-051'(suite) -> [];
'not-wf-sa-051'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-051.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-050'(suite) -> [];
'not-wf-sa-050'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-050.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-049'(suite) -> [];
'not-wf-sa-049'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-049.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-048'(suite) -> [];
'not-wf-sa-048'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-048.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-047'(suite) -> [];
'not-wf-sa-047'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-047.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-046'(suite) -> [];
'not-wf-sa-046'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-046.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-045'(suite) -> [];
'not-wf-sa-045'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-045.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-044'(suite) -> [];
'not-wf-sa-044'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-044.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-043'(suite) -> [];
'not-wf-sa-043'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-043.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-042'(suite) -> [];
'not-wf-sa-042'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-042.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-041'(suite) -> [];
'not-wf-sa-041'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-041.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-040'(suite) -> [];
'not-wf-sa-040'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-040.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-039'(suite) -> [];
'not-wf-sa-039'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-039.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-038'(suite) -> [];
'not-wf-sa-038'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-038.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-037'(suite) -> [];
'not-wf-sa-037'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-037.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-036'(suite) -> [];
'not-wf-sa-036'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-036.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-035'(suite) -> [];
'not-wf-sa-035'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-035.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-034'(suite) -> [];
'not-wf-sa-034'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-034.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-033'(suite) -> [];
'not-wf-sa-033'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-033.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-032'(suite) -> [];
'not-wf-sa-032'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-032.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-031'(suite) -> [];
'not-wf-sa-031'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-031.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-030'(suite) -> [];
'not-wf-sa-030'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-030.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-029'(suite) -> [];
'not-wf-sa-029'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-029.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-028'(suite) -> [];
'not-wf-sa-028'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-028.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-027'(suite) -> [];
'not-wf-sa-027'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-027.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-026'(suite) -> [];
'not-wf-sa-026'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-026.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-025'(suite) -> [];
'not-wf-sa-025'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-025.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-024'(suite) -> [];
'not-wf-sa-024'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-024.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-023'(suite) -> [];
'not-wf-sa-023'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-023.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-022'(suite) -> [];
'not-wf-sa-022'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-022.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-021'(suite) -> [];
'not-wf-sa-021'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-021.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-020'(suite) -> [];
'not-wf-sa-020'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-020.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-019'(suite) -> [];
'not-wf-sa-019'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-019.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-018'(suite) -> [];
'not-wf-sa-018'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-018.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-017'(suite) -> [];
'not-wf-sa-017'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-017.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-016'(suite) -> [];
'not-wf-sa-016'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-016.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-015'(suite) -> [];
'not-wf-sa-015'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-015.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-014'(suite) -> [];
'not-wf-sa-014'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-014.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-013'(suite) -> [];
'not-wf-sa-013'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-013.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-012'(suite) -> [];
'not-wf-sa-012'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-012.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-011'(suite) -> [];
'not-wf-sa-011'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-011.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-010'(suite) -> [];
'not-wf-sa-010'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-010.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-009'(suite) -> [];
'not-wf-sa-009'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-009.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-008'(suite) -> [];
'not-wf-sa-008'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-008.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-007'(suite) -> [];
'not-wf-sa-007'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-007.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-006'(suite) -> [];
'not-wf-sa-006'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-006.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-005'(suite) -> [];
'not-wf-sa-005'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-005.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-004'(suite) -> [];
'not-wf-sa-004'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-004.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-003'(suite) -> [];
'not-wf-sa-003'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-003.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-002'(suite) -> [];
'not-wf-sa-002'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-002.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'not-wf-sa-001'(suite) -> [];
'not-wf-sa-001'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),xmltest,"not-wf-sa-001.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

%%----------------------------------------------------------------------

'japanese-weekly-utf-8'(suite) -> [];
'japanese-weekly-utf-8'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),japanese,"japanese-weekly-utf-8.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["UTF-8 encoding of japanese characters"]}.

'japanese-weekly-utf-16'(suite) -> [];
'japanese-weekly-utf-16'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),japanese,"japanese-weekly-utf-16.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Test support for UTF-16 encoding, and XML names which contain Japanese characters."]}.

'japanese-weekly-shift_jis'(suite) -> [];
'japanese-weekly-shift_jis'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),japanese,"japanese-weekly-shift_jis.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'japanese-weekly-little'(suite) -> [];
'japanese-weekly-little'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),japanese,"japanese-weekly-little.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Test support for little-endian UTF-16 encoding, and XML names which contain Japanese characters."]}.

'japanese-weekly-iso-2022-jp'(suite) -> [];
'japanese-weekly-iso-2022-jp'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),japanese,"japanese-weekly-iso-2022-jp.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'japanese-weekly-euc-jp'(suite) -> [];
'japanese-weekly-euc-jp'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),japanese,"japanese-weekly-euc-jp.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'japanese-pr-xml-utf-8'(suite) -> [];
'japanese-pr-xml-utf-8'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),japanese,"japanese-pr-xml-utf-8.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Test support for UTF-8 text which relies on Japanese characters"]}.

'japanese-pr-xml-utf-16'(suite) -> [];
'japanese-pr-xml-utf-16'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),japanese,"japanese-pr-xml-utf-16.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Test support UTF-16 text which relies on Japanese characters."]}.

'japanese-pr-xml-shift_jis'(suite) -> [];
'japanese-pr-xml-shift_jis'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),japanese,"japanese-pr-xml-shift_jis.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'japanese-pr-xml-little'(suite) -> [];
'japanese-pr-xml-little'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),japanese,"japanese-pr-xml-little.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Test support for little-endian UTF-16 text which relies on Japanese characters."]}.

'japanese-pr-xml-iso-2022-jp'(suite) -> [];
'japanese-pr-xml-iso-2022-jp'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),japanese,"japanese-pr-xml-iso-2022-jp.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'japanese-pr-xml-euc-jp'(suite) -> [];
'japanese-pr-xml-euc-jp'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),japanese,"japanese-pr-xml-euc-jp.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

%%----------------------------------------------------------------------

'o-p11pass1'(suite) -> [];
'o-p11pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p11pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p76fail4'(suite) -> [];
'o-p76fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p76fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p76fail3'(suite) -> [];
'o-p76fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p76fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p76fail2'(suite) -> [];
'o-p76fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p76fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p76fail1'(suite) -> [];
'o-p76fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p76fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p75fail6'(suite) -> [];
'o-p75fail6'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p75fail6.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p75fail5'(suite) -> [];
'o-p75fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p75fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p75fail4'(suite) -> [];
'o-p75fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p75fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p75fail3'(suite) -> [];
'o-p75fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p75fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p75fail2'(suite) -> [];
'o-p75fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p75fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p75fail1'(suite) -> [];
'o-p75fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p75fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p74fail3'(suite) -> [];
'o-p74fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p74fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p74fail2'(suite) -> [];
'o-p74fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p74fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p74fail1'(suite) -> [];
'o-p74fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p74fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p73fail5'(suite) -> [];
'o-p73fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p73fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p73fail4'(suite) -> [];
'o-p73fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p73fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p73fail3'(suite) -> [];
'o-p73fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p73fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p73fail2'(suite) -> [];
'o-p73fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p73fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p73fail1'(suite) -> [];
'o-p73fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p73fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p72fail4'(suite) -> [];
'o-p72fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p72fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p72fail3'(suite) -> [];
'o-p72fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p72fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p72fail2'(suite) -> [];
'o-p72fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p72fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p72fail1'(suite) -> [];
'o-p72fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p72fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p71fail4'(suite) -> [];
'o-p71fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p71fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p71fail3'(suite) -> [];
'o-p71fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p71fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p71fail2'(suite) -> [];
'o-p71fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p71fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p71fail1'(suite) -> [];
'o-p71fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p71fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p70fail1'(suite) -> [];
'o-p70fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p70fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p69fail3'(suite) -> [];
'o-p69fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p69fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p69fail2'(suite) -> [];
'o-p69fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p69fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p69fail1'(suite) -> [];
'o-p69fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p69fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p68fail3'(suite) -> [];
'o-p68fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p68fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p68fail2'(suite) -> [];
'o-p68fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p68fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p68fail1'(suite) -> [];
'o-p68fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p68fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p66fail6'(suite) -> [];
'o-p66fail6'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p66fail6.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p66fail5'(suite) -> [];
'o-p66fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p66fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p66fail4'(suite) -> [];
'o-p66fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p66fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p66fail3'(suite) -> [];
'o-p66fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p66fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p66fail2'(suite) -> [];
'o-p66fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p66fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p66fail1'(suite) -> [];
'o-p66fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p66fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p64fail2'(suite) -> [];
'o-p64fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p64fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p64fail1'(suite) -> [];
'o-p64fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p64fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p63fail2'(suite) -> [];
'o-p63fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p63fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p63fail1'(suite) -> [];
'o-p63fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p63fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p62fail2'(suite) -> [];
'o-p62fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p62fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p62fail1'(suite) -> [];
'o-p62fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p62fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p61fail1'(suite) -> [];
'o-p61fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p61fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p60fail5'(suite) -> [];
'o-p60fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p60fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p60fail4'(suite) -> [];
'o-p60fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p60fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p60fail3'(suite) -> [];
'o-p60fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p60fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p60fail2'(suite) -> [];
'o-p60fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p60fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p60fail1'(suite) -> [];
'o-p60fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p60fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p59fail3'(suite) -> [];
'o-p59fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p59fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p59fail2'(suite) -> [];
'o-p59fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p59fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p59fail1'(suite) -> [];
'o-p59fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p59fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p58fail8'(suite) -> [];
'o-p58fail8'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p58fail8.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p58fail7'(suite) -> [];
'o-p58fail7'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p58fail7.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p58fail6'(suite) -> [];
'o-p58fail6'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p58fail6.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p58fail5'(suite) -> [];
'o-p58fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p58fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p58fail4'(suite) -> [];
'o-p58fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p58fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p58fail3'(suite) -> [];
'o-p58fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p58fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p58fail2'(suite) -> [];
'o-p58fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p58fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p58fail1'(suite) -> [];
'o-p58fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p58fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p57fail1'(suite) -> [];
'o-p57fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p57fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p56fail5'(suite) -> [];
'o-p56fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p56fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p56fail4'(suite) -> [];
'o-p56fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p56fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p56fail3'(suite) -> [];
'o-p56fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p56fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p56fail2'(suite) -> [];
'o-p56fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p56fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p56fail1'(suite) -> [];
'o-p56fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p56fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p55fail1'(suite) -> [];
'o-p55fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p55fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p54fail1'(suite) -> [];
'o-p54fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p54fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p53fail5'(suite) -> [];
'o-p53fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p53fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p53fail4'(suite) -> [];
'o-p53fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p53fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p53fail3'(suite) -> [];
'o-p53fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p53fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p53fail2'(suite) -> [];
'o-p53fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p53fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p53fail1'(suite) -> [];
'o-p53fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p53fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p52fail2'(suite) -> [];
'o-p52fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p52fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p52fail1'(suite) -> [];
'o-p52fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p52fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p51fail7'(suite) -> [];
'o-p51fail7'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p51fail7.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p51fail6'(suite) -> [];
'o-p51fail6'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p51fail6.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p51fail5'(suite) -> [];
'o-p51fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p51fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p51fail4'(suite) -> [];
'o-p51fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p51fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p51fail3'(suite) -> [];
'o-p51fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p51fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p51fail2'(suite) -> [];
'o-p51fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p51fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p51fail1'(suite) -> [];
'o-p51fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p51fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p50fail1'(suite) -> [];
'o-p50fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p50fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p49fail1'(suite) -> [];
'o-p49fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p49fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p48fail2'(suite) -> [];
'o-p48fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p48fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p48fail1'(suite) -> [];
'o-p48fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p48fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p47fail4'(suite) -> [];
'o-p47fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p47fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p47fail3'(suite) -> [];
'o-p47fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p47fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p47fail2'(suite) -> [];
'o-p47fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p47fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p47fail1'(suite) -> [];
'o-p47fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p47fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p46fail6'(suite) -> [];
'o-p46fail6'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p46fail6.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p46fail5'(suite) -> [];
'o-p46fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p46fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p46fail4'(suite) -> [];
'o-p46fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p46fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p46fail3'(suite) -> [];
'o-p46fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p46fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p46fail2'(suite) -> [];
'o-p46fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p46fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p46fail1'(suite) -> [];
'o-p46fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p46fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p45fail4'(suite) -> [];
'o-p45fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p45fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p45fail3'(suite) -> [];
'o-p45fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p45fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p45fail2'(suite) -> [];
'o-p45fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p45fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p45fail1'(suite) -> [];
'o-p45fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p45fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p44fail5'(suite) -> [];
'o-p44fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p44fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p44fail4'(suite) -> [];
'o-p44fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p44fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p44fail3'(suite) -> [];
'o-p44fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p44fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p44fail2'(suite) -> [];
'o-p44fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p44fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p44fail1'(suite) -> [];
'o-p44fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p44fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p43fail3'(suite) -> [];
'o-p43fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p43fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p43fail2'(suite) -> [];
'o-p43fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p43fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p43fail1'(suite) -> [];
'o-p43fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p43fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p42fail3'(suite) -> [];
'o-p42fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p42fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p42fail2'(suite) -> [];
'o-p42fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p42fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p42fail1'(suite) -> [];
'o-p42fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p42fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p41fail3'(suite) -> [];
'o-p41fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p41fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p41fail2'(suite) -> [];
'o-p41fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p41fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p41fail1'(suite) -> [];
'o-p41fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p41fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p40fail4'(suite) -> [];
'o-p40fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p40fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p40fail3'(suite) -> [];
'o-p40fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p40fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p40fail2'(suite) -> [];
'o-p40fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p40fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p40fail1'(suite) -> [];
'o-p40fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p40fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p39fail5'(suite) -> [];
'o-p39fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p39fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p39fail4'(suite) -> [];
'o-p39fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p39fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p39fail3'(suite) -> [];
'o-p39fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p39fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p39fail2'(suite) -> [];
'o-p39fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p39fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p39fail1'(suite) -> [];
'o-p39fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p39fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p32fail5'(suite) -> [];
'o-p32fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p32fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p32fail4'(suite) -> [];
'o-p32fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p32fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p32fail3'(suite) -> [];
'o-p32fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p32fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p32fail2'(suite) -> [];
'o-p32fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p32fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p32fail1'(suite) -> [];
'o-p32fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p32fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p31fail1'(suite) -> [];
'o-p31fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p31fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p30fail1'(suite) -> [];
'o-p30fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p30fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p29fail1'(suite) -> [];
'o-p29fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p29fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p28fail1'(suite) -> [];
'o-p28fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p28fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p27fail1'(suite) -> [];
'o-p27fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p27fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p26fail2'(suite) -> [];
'o-p26fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p26fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p26fail1'(suite) -> [];
'o-p26fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p26fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p25fail1'(suite) -> [];
'o-p25fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p25fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p24fail2'(suite) -> [];
'o-p24fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p24fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p24fail1'(suite) -> [];
'o-p24fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p24fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p23fail5'(suite) -> [];
'o-p23fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p23fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p23fail4'(suite) -> [];
'o-p23fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p23fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p23fail3'(suite) -> [];
'o-p23fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p23fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p23fail2'(suite) -> [];
'o-p23fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p23fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p23fail1'(suite) -> [];
'o-p23fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p23fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p22fail2'(suite) -> [];
'o-p22fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p22fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p22fail1'(suite) -> [];
'o-p22fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p22fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p18fail3'(suite) -> [];
'o-p18fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p18fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p18fail2'(suite) -> [];
'o-p18fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p18fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p18fail1'(suite) -> [];
'o-p18fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p18fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p16fail3'(suite) -> [];
'o-p16fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p16fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p16fail2'(suite) -> [];
'o-p16fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p16fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p16fail1'(suite) -> [];
'o-p16fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p16fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p15fail3'(suite) -> [];
'o-p15fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p15fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p15fail2'(suite) -> [];
'o-p15fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p15fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p15fail1'(suite) -> [];
'o-p15fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p15fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p14fail3'(suite) -> [];
'o-p14fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p14fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p14fail2'(suite) -> [];
'o-p14fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p14fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p14fail1'(suite) -> [];
'o-p14fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p14fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p12fail7'(suite) -> [];
'o-p12fail7'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p12fail7.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p12fail6'(suite) -> [];
'o-p12fail6'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p12fail6.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p12fail5'(suite) -> [];
'o-p12fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p12fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p12fail4'(suite) -> [];
'o-p12fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p12fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p12fail3'(suite) -> [];
'o-p12fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p12fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p12fail2'(suite) -> [];
'o-p12fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p12fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p12fail1'(suite) -> [];
'o-p12fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p12fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p11fail2'(suite) -> [];
'o-p11fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p11fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p11fail1'(suite) -> [];
'o-p11fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p11fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p10fail3'(suite) -> [];
'o-p10fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p10fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p10fail2'(suite) -> [];
'o-p10fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p10fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p10fail1'(suite) -> [];
'o-p10fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p10fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p09fail5'(suite) -> [];
'o-p09fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p09fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p09fail4'(suite) -> [];
'o-p09fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p09fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p09fail3'(suite) -> [];
'o-p09fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p09fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p09fail2'(suite) -> [];
'o-p09fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p09fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p09fail1'(suite) -> [];
'o-p09fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p09fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p05fail5'(suite) -> [];
'o-p05fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p05fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p05fail4'(suite) -> [];
'o-p05fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p05fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p05fail3'(suite) -> [];
'o-p05fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p05fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p05fail2'(suite) -> [];
'o-p05fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p05fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p05fail1'(suite) -> [];
'o-p05fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p05fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p04fail3'(suite) -> [];
'o-p04fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p04fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p04fail2'(suite) -> [];
'o-p04fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p04fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p04fail1'(suite) -> [];
'o-p04fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p04fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail9'(suite) -> [];
'o-p03fail9'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail9.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail8'(suite) -> [];
'o-p03fail8'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail8.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail7'(suite) -> [];
'o-p03fail7'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail7.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail5'(suite) -> [];
'o-p03fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail4'(suite) -> [];
'o-p03fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail3'(suite) -> [];
'o-p03fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail29'(suite) -> [];
'o-p03fail29'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail29.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail28'(suite) -> [];
'o-p03fail28'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail28.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail27'(suite) -> [];
'o-p03fail27'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail27.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail26'(suite) -> [];
'o-p03fail26'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail26.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail25'(suite) -> [];
'o-p03fail25'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail25.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail24'(suite) -> [];
'o-p03fail24'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail24.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail23'(suite) -> [];
'o-p03fail23'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail23.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail22'(suite) -> [];
'o-p03fail22'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail22.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail21'(suite) -> [];
'o-p03fail21'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail21.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail20'(suite) -> [];
'o-p03fail20'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail20.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail2'(suite) -> [];
'o-p03fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail19'(suite) -> [];
'o-p03fail19'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail19.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail18'(suite) -> [];
'o-p03fail18'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail18.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail17'(suite) -> [];
'o-p03fail17'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail17.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail16'(suite) -> [];
'o-p03fail16'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail16.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail15'(suite) -> [];
'o-p03fail15'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail15.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail14'(suite) -> [];
'o-p03fail14'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail14.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail13'(suite) -> [];
'o-p03fail13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail12'(suite) -> [];
'o-p03fail12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail11'(suite) -> [];
'o-p03fail11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail10'(suite) -> [];
'o-p03fail10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p03fail1'(suite) -> [];
'o-p03fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail9'(suite) -> [];
'o-p02fail9'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail9.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail8'(suite) -> [];
'o-p02fail8'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail8.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail7'(suite) -> [];
'o-p02fail7'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail7.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail6'(suite) -> [];
'o-p02fail6'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail6.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail5'(suite) -> [];
'o-p02fail5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail4'(suite) -> [];
'o-p02fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail31'(suite) -> [];
'o-p02fail31'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail31.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail30'(suite) -> [];
'o-p02fail30'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail30.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail3'(suite) -> [];
'o-p02fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail29'(suite) -> [];
'o-p02fail29'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail29.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail28'(suite) -> [];
'o-p02fail28'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail28.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail27'(suite) -> [];
'o-p02fail27'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail27.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail26'(suite) -> [];
'o-p02fail26'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail26.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail25'(suite) -> [];
'o-p02fail25'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail25.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail24'(suite) -> [];
'o-p02fail24'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail24.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail23'(suite) -> [];
'o-p02fail23'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail23.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail22'(suite) -> [];
'o-p02fail22'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail22.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail21'(suite) -> [];
'o-p02fail21'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail21.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail20'(suite) -> [];
'o-p02fail20'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail20.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail2'(suite) -> [];
'o-p02fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail19'(suite) -> [];
'o-p02fail19'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail19.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail18'(suite) -> [];
'o-p02fail18'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail18.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail17'(suite) -> [];
'o-p02fail17'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail17.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail16'(suite) -> [];
'o-p02fail16'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail16.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail15'(suite) -> [];
'o-p02fail15'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail15.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail14'(suite) -> [];
'o-p02fail14'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail14.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail13'(suite) -> [];
'o-p02fail13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail12'(suite) -> [];
'o-p02fail12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail11'(suite) -> [];
'o-p02fail11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail10'(suite) -> [];
'o-p02fail10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p02fail1'(suite) -> [];
'o-p02fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p02fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p01fail4'(suite) -> [];
'o-p01fail4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p01fail4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p01fail3'(suite) -> [];
'o-p01fail3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p01fail3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p01fail2'(suite) -> [];
'o-p01fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p01fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p01fail1'(suite) -> [];
'o-p01fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p01fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-e2'(suite) -> [];
'o-e2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-e2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p75pass1'(suite) -> [];
'o-p75pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p75pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p74pass1'(suite) -> [];
'o-p74pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p74pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p66pass1'(suite) -> [];
'o-p66pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p66pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p44pass5'(suite) -> [];
'o-p44pass5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p44pass5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p44pass4'(suite) -> [];
'o-p44pass4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p44pass4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p44pass3'(suite) -> [];
'o-p44pass3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p44pass3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p44pass2'(suite) -> [];
'o-p44pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p44pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p44pass1'(suite) -> [];
'o-p44pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p44pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p42pass2'(suite) -> [];
'o-p42pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p42pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p42pass1'(suite) -> [];
'o-p42pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p42pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p41pass2'(suite) -> [];
'o-p41pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p41pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p41pass1'(suite) -> [];
'o-p41pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p41pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p40pass4'(suite) -> [];
'o-p40pass4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p40pass4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p40pass3'(suite) -> [];
'o-p40pass3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p40pass3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p40pass2'(suite) -> [];
'o-p40pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p40pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p40pass1'(suite) -> [];
'o-p40pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p40pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p39pass2'(suite) -> [];
'o-p39pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p39pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p39pass1'(suite) -> [];
'o-p39pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p39pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p32pass2'(suite) -> [];
'o-p32pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p32pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p32pass1'(suite) -> [];
'o-p32pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p32pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p27pass4'(suite) -> [];
'o-p27pass4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p27pass4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p27pass3'(suite) -> [];
'o-p27pass3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p27pass3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p27pass2'(suite) -> [];
'o-p27pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p27pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p27pass1'(suite) -> [];
'o-p27pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p27pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p26pass1'(suite) -> [];
'o-p26pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p26pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p25pass2'(suite) -> [];
'o-p25pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p25pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p25pass1'(suite) -> [];
'o-p25pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p25pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p24pass4'(suite) -> [];
'o-p24pass4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p24pass4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p24pass3'(suite) -> [];
'o-p24pass3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p24pass3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p24pass2'(suite) -> [];
'o-p24pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p24pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p24pass1'(suite) -> [];
'o-p24pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p24pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p23pass4'(suite) -> [];
'o-p23pass4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p23pass4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p23pass3'(suite) -> [];
'o-p23pass3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p23pass3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p23pass2'(suite) -> [];
'o-p23pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p23pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p23pass1'(suite) -> [];
'o-p23pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p23pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p22pass3'(suite) -> [];
'o-p22pass3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p22pass3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p22pass2'(suite) -> [];
'o-p22pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p22pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p22pass1'(suite) -> [];
'o-p22pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p22pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p18pass1'(suite) -> [];
'o-p18pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p18pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p16pass3'(suite) -> [];
'o-p16pass3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p16pass3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p16pass2'(suite) -> [];
'o-p16pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p16pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p16pass1'(suite) -> [];
'o-p16pass1'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p16pass1.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Hard to interpret the meaning of the XML1.0 spec. See section 2.6 and 2.3."]}.

'o-p15pass1'(suite) -> [];
'o-p15pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p15pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p14pass1'(suite) -> [];
'o-p14pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p14pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p10pass1'(suite) -> [];
'o-p10pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p10pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p08fail2'(suite) -> [];
'o-p08fail2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p08fail2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p08fail1'(suite) -> [];
'o-p08fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p08fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p06fail1'(suite) -> [];
'o-p06fail1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p06fail1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p05pass1'(suite) -> [];
'o-p05pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p05pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p04pass1'(suite) -> [];
'o-p04pass1'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p04pass1.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Fails to handle name containing characters > x#ff, since they are converted to atoms"]}.
'o-p03pass1'(suite) -> [];
'o-p03pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p03pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p01pass3'(suite) -> [];
'o-p01pass3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p01pass3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p01pass1'(suite) -> [];
'o-p01pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p01pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p76pass1'(suite) -> [];
'o-p76pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p76pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p73pass1'(suite) -> [];
'o-p73pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p73pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p72pass1'(suite) -> [];
'o-p72pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p72pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p71pass1'(suite) -> [];
'o-p71pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p71pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p70pass1'(suite) -> [];
'o-p70pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p70pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p69pass1'(suite) -> [];
'o-p69pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p69pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p68pass1'(suite) -> [];
'o-p68pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p68pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p64pass1'(suite) -> [];
'o-p64pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p64pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p63pass1'(suite) -> [];
'o-p63pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p63pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p62pass1'(suite) -> [];
'o-p62pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p62pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p61pass1'(suite) -> [];
'o-p61pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p61pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p60pass1'(suite) -> [];
'o-p60pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p60pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p59pass1'(suite) -> [];
'o-p59pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p59pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p58pass1'(suite) -> [];
'o-p58pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p58pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p57pass1'(suite) -> [];
'o-p57pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p57pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p56pass1'(suite) -> [];
'o-p56pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p56pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p55pass1'(suite) -> [];
'o-p55pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p55pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p54pass1'(suite) -> [];
'o-p54pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p54pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p53pass1'(suite) -> [];
'o-p53pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p53pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p52pass1'(suite) -> [];
'o-p52pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p52pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p51pass1'(suite) -> [];
'o-p51pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p51pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p50pass1'(suite) -> [];
'o-p50pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p50pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p49pass1'(suite) -> [];
'o-p49pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p49pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p48pass1'(suite) -> [];
'o-p48pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p48pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p47pass1'(suite) -> [];
'o-p47pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p47pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p46pass1'(suite) -> [];
'o-p46pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p46pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p45pass1'(suite) -> [];
'o-p45pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p45pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p43pass1'(suite) -> [];
'o-p43pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p43pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p31pass2'(suite) -> [];
'o-p31pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p31pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p31pass1'(suite) -> [];
'o-p31pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p31pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p30pass2'(suite) -> [];
'o-p30pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p30pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p30pass1'(suite) -> [];
'o-p30pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p30pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p29pass1'(suite) -> [];
'o-p29pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p29pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p28pass5'(suite) -> [];
'o-p28pass5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p28pass5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p28pass4'(suite) -> [];
'o-p28pass4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p28pass4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p28pass3'(suite) -> [];
'o-p28pass3'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p28pass3.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p28pass1'(suite) -> [];
'o-p28pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p28pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p22pass6'(suite) -> [];
'o-p22pass6'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p22pass6.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p22pass5'(suite) -> [];
'o-p22pass5'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p22pass5.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p22pass4'(suite) -> [];
'o-p22pass4'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p22pass4.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p12pass1'(suite) -> [];
'o-p12pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p12pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p09pass1'(suite) -> [];
'o-p09pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p09pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p08pass1'(suite) -> [];
'o-p08pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p08pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p07pass1'(suite) -> [];
'o-p07pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p07pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p06pass1'(suite) -> [];
'o-p06pass1'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p06pass1.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'o-p01pass2'(suite) -> [];
'o-p01pass2'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),oasis,"o-p01pass2.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

%%----------------------------------------------------------------------

'ibm-invalid-P76-ibm76i01'(suite) -> [];
'ibm-invalid-P76-ibm76i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P76-ibm76i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P69-ibm69i04'(suite) -> [];
'ibm-invalid-P69-ibm69i04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P69-ibm69i04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P69-ibm69i03'(suite) -> [];
'ibm-invalid-P69-ibm69i03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P69-ibm69i03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P69-ibm69i02'(suite) -> [];
'ibm-invalid-P69-ibm69i02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P69-ibm69i02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P69-ibm69i01'(suite) -> [];
'ibm-invalid-P69-ibm69i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P69-ibm69i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P68-ibm68i04'(suite) -> [];
'ibm-invalid-P68-ibm68i04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P68-ibm68i04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P68-ibm68i03'(suite) -> [];
'ibm-invalid-P68-ibm68i03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P68-ibm68i03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P68-ibm68i02'(suite) -> [];
'ibm-invalid-P68-ibm68i02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P68-ibm68i02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P68-ibm68i01'(suite) -> [];
'ibm-invalid-P68-ibm68i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P68-ibm68i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P60-ibm60i04'(suite) -> [];
'ibm-invalid-P60-ibm60i04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P60-ibm60i04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P60-ibm60i03'(suite) -> [];
'ibm-invalid-P60-ibm60i03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P60-ibm60i03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P60-ibm60i02'(suite) -> [];
'ibm-invalid-P60-ibm60i02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P60-ibm60i02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P60-ibm60i01'(suite) -> [];
'ibm-invalid-P60-ibm60i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P60-ibm60i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P59-ibm59i01'(suite) -> [];
'ibm-invalid-P59-ibm59i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P59-ibm59i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P58-ibm58i02'(suite) -> [];
'ibm-invalid-P58-ibm58i02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P58-ibm58i02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P58-ibm58i01'(suite) -> [];
'ibm-invalid-P58-ibm58i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P58-ibm58i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i18'(suite) -> [];
'ibm-invalid-P56-ibm56i18'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i18.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i17'(suite) -> [];
'ibm-invalid-P56-ibm56i17'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i17.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i16'(suite) -> [];
'ibm-invalid-P56-ibm56i16'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i16.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i15'(suite) -> [];
'ibm-invalid-P56-ibm56i15'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i15.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i14'(suite) -> [];
'ibm-invalid-P56-ibm56i14'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i14.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i13'(suite) -> [];
'ibm-invalid-P56-ibm56i13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i12'(suite) -> [];
'ibm-invalid-P56-ibm56i12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i11'(suite) -> [];
'ibm-invalid-P56-ibm56i11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i10'(suite) -> [];
'ibm-invalid-P56-ibm56i10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i09'(suite) -> [];
'ibm-invalid-P56-ibm56i09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i08'(suite) -> [];
'ibm-invalid-P56-ibm56i08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i07'(suite) -> [];
'ibm-invalid-P56-ibm56i07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i06'(suite) -> [];
'ibm-invalid-P56-ibm56i06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i05'(suite) -> [];
'ibm-invalid-P56-ibm56i05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i03'(suite) -> [];
'ibm-invalid-P56-ibm56i03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i02'(suite) -> [];
'ibm-invalid-P56-ibm56i02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P56-ibm56i01'(suite) -> [];
'ibm-invalid-P56-ibm56i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P56-ibm56i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P51-ibm51i03'(suite) -> [];
'ibm-invalid-P51-ibm51i03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P51-ibm51i03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P51-ibm51i01'(suite) -> [];
'ibm-invalid-P51-ibm51i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P51-ibm51i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P50-ibm50i01'(suite) -> [];
'ibm-invalid-P50-ibm50i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P50-ibm50i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P49-ibm49i01'(suite) -> [];
'ibm-invalid-P49-ibm49i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P49-ibm49i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P45-ibm45i01'(suite) -> [];
'ibm-invalid-P45-ibm45i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P45-ibm45i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P41-ibm41i02'(suite) -> [];
'ibm-invalid-P41-ibm41i02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P41-ibm41i02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P41-ibm41i01'(suite) -> [];
'ibm-invalid-P41-ibm41i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P41-ibm41i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P39-ibm39i04'(suite) -> [];
'ibm-invalid-P39-ibm39i04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P39-ibm39i04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P39-ibm39i03'(suite) -> [];
'ibm-invalid-P39-ibm39i03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P39-ibm39i03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P39-ibm39i02'(suite) -> [];
'ibm-invalid-P39-ibm39i02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P39-ibm39i02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P39-ibm39i01'(suite) -> [];
'ibm-invalid-P39-ibm39i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P39-ibm39i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P32-ibm32i04'(suite) -> [];
'ibm-invalid-P32-ibm32i04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P32-ibm32i04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P32-ibm32i03'(suite) -> [];
'ibm-invalid-P32-ibm32i03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P32-ibm32i03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P32-ibm32i01'(suite) -> [];
'ibm-invalid-P32-ibm32i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P32-ibm32i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-invalid-P28-ibm28i01'(suite) -> [];
'ibm-invalid-P28-ibm28i01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-invalid-P28-ibm28i01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

%%----------------------------------------------------------------------

'ibm-not-wf-P89-ibm89n12'(suite) -> [];
'ibm-not-wf-P89-ibm89n12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P89-ibm89n12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P89-ibm89n11'(suite) -> [];
'ibm-not-wf-P89-ibm89n11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P89-ibm89n11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P89-ibm89n10'(suite) -> [];
'ibm-not-wf-P89-ibm89n10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P89-ibm89n10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P89-ibm89n09'(suite) -> [];
'ibm-not-wf-P89-ibm89n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P89-ibm89n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P89-ibm89n08'(suite) -> [];
'ibm-not-wf-P89-ibm89n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P89-ibm89n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P89-ibm89n07'(suite) -> [];
'ibm-not-wf-P89-ibm89n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P89-ibm89n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P89-ibm89n06'(suite) -> [];
'ibm-not-wf-P89-ibm89n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P89-ibm89n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P89-ibm89n05'(suite) -> [];
'ibm-not-wf-P89-ibm89n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P89-ibm89n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P89-ibm89n04'(suite) -> [];
'ibm-not-wf-P89-ibm89n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P89-ibm89n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P89-ibm89n03'(suite) -> [];
'ibm-not-wf-P89-ibm89n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P89-ibm89n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P89-ibm89n02'(suite) -> [];
'ibm-not-wf-P89-ibm89n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P89-ibm89n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P89-ibm89n01'(suite) -> [];
'ibm-not-wf-P89-ibm89n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P89-ibm89n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n16'(suite) -> [];
'ibm-not-wf-P88-ibm88n16'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n16.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n15'(suite) -> [];
'ibm-not-wf-P88-ibm88n15'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n15.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n14'(suite) -> [];
'ibm-not-wf-P88-ibm88n14'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n14.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n13'(suite) -> [];
'ibm-not-wf-P88-ibm88n13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n12'(suite) -> [];
'ibm-not-wf-P88-ibm88n12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n11'(suite) -> [];
'ibm-not-wf-P88-ibm88n11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n10'(suite) -> [];
'ibm-not-wf-P88-ibm88n10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n09'(suite) -> [];
'ibm-not-wf-P88-ibm88n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n08'(suite) -> [];
'ibm-not-wf-P88-ibm88n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n06'(suite) -> [];
'ibm-not-wf-P88-ibm88n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n05'(suite) -> [];
'ibm-not-wf-P88-ibm88n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n04'(suite) -> [];
'ibm-not-wf-P88-ibm88n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n03'(suite) -> [];
'ibm-not-wf-P88-ibm88n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n02'(suite) -> [];
'ibm-not-wf-P88-ibm88n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P88-ibm88n01'(suite) -> [];
'ibm-not-wf-P88-ibm88n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P88-ibm88n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n85'(suite) -> [];
'ibm-not-wf-P87-ibm87n85'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n85.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n84'(suite) -> [];
'ibm-not-wf-P87-ibm87n84'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n84.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n83'(suite) -> [];
'ibm-not-wf-P87-ibm87n83'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n83.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n82'(suite) -> [];
'ibm-not-wf-P87-ibm87n82'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n82.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n81'(suite) -> [];
'ibm-not-wf-P87-ibm87n81'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n81.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n80'(suite) -> [];
'ibm-not-wf-P87-ibm87n80'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n80.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n79'(suite) -> [];
'ibm-not-wf-P87-ibm87n79'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n79.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n78'(suite) -> [];
'ibm-not-wf-P87-ibm87n78'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n78.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n77'(suite) -> [];
'ibm-not-wf-P87-ibm87n77'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n77.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n76'(suite) -> [];
'ibm-not-wf-P87-ibm87n76'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n76.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n75'(suite) -> [];
'ibm-not-wf-P87-ibm87n75'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n75.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n74'(suite) -> [];
'ibm-not-wf-P87-ibm87n74'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n74.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n73'(suite) -> [];
'ibm-not-wf-P87-ibm87n73'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n73.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n72'(suite) -> [];
'ibm-not-wf-P87-ibm87n72'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n72.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n71'(suite) -> [];
'ibm-not-wf-P87-ibm87n71'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n71.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n70'(suite) -> [];
'ibm-not-wf-P87-ibm87n70'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n70.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n69'(suite) -> [];
'ibm-not-wf-P87-ibm87n69'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n69.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n68'(suite) -> [];
'ibm-not-wf-P87-ibm87n68'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n68.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n67'(suite) -> [];
'ibm-not-wf-P87-ibm87n67'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n67.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n66'(suite) -> [];
'ibm-not-wf-P87-ibm87n66'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n66.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n64'(suite) -> [];
'ibm-not-wf-P87-ibm87n64'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n64.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n63'(suite) -> [];
'ibm-not-wf-P87-ibm87n63'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n63.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n62'(suite) -> [];
'ibm-not-wf-P87-ibm87n62'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n62.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n61'(suite) -> [];
'ibm-not-wf-P87-ibm87n61'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n61.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n60'(suite) -> [];
'ibm-not-wf-P87-ibm87n60'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n60.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n59'(suite) -> [];
'ibm-not-wf-P87-ibm87n59'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n59.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n58'(suite) -> [];
'ibm-not-wf-P87-ibm87n58'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n58.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n57'(suite) -> [];
'ibm-not-wf-P87-ibm87n57'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n57.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n56'(suite) -> [];
'ibm-not-wf-P87-ibm87n56'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n56.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n55'(suite) -> [];
'ibm-not-wf-P87-ibm87n55'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n55.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n54'(suite) -> [];
'ibm-not-wf-P87-ibm87n54'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n54.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n53'(suite) -> [];
'ibm-not-wf-P87-ibm87n53'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n53.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n52'(suite) -> [];
'ibm-not-wf-P87-ibm87n52'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n52.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n51'(suite) -> [];
'ibm-not-wf-P87-ibm87n51'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n51.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n50'(suite) -> [];
'ibm-not-wf-P87-ibm87n50'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n50.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n49'(suite) -> [];
'ibm-not-wf-P87-ibm87n49'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n49.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n48'(suite) -> [];
'ibm-not-wf-P87-ibm87n48'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n48.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n47'(suite) -> [];
'ibm-not-wf-P87-ibm87n47'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n47.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n46'(suite) -> [];
'ibm-not-wf-P87-ibm87n46'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n46.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n45'(suite) -> [];
'ibm-not-wf-P87-ibm87n45'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n45.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n44'(suite) -> [];
'ibm-not-wf-P87-ibm87n44'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n44.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n43'(suite) -> [];
'ibm-not-wf-P87-ibm87n43'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n43.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n42'(suite) -> [];
'ibm-not-wf-P87-ibm87n42'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n42.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n41'(suite) -> [];
'ibm-not-wf-P87-ibm87n41'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n41.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n40'(suite) -> [];
'ibm-not-wf-P87-ibm87n40'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n40.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n39'(suite) -> [];
'ibm-not-wf-P87-ibm87n39'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n39.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n38'(suite) -> [];
'ibm-not-wf-P87-ibm87n38'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n38.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n37'(suite) -> [];
'ibm-not-wf-P87-ibm87n37'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n37.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n36'(suite) -> [];
'ibm-not-wf-P87-ibm87n36'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n36.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n35'(suite) -> [];
'ibm-not-wf-P87-ibm87n35'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n35.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n34'(suite) -> [];
'ibm-not-wf-P87-ibm87n34'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n34.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n33'(suite) -> [];
'ibm-not-wf-P87-ibm87n33'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n33.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n32'(suite) -> [];
'ibm-not-wf-P87-ibm87n32'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n32.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n31'(suite) -> [];
'ibm-not-wf-P87-ibm87n31'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n31.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n30'(suite) -> [];
'ibm-not-wf-P87-ibm87n30'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n30.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n29'(suite) -> [];
'ibm-not-wf-P87-ibm87n29'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n29.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n28'(suite) -> [];
'ibm-not-wf-P87-ibm87n28'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n28.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n27'(suite) -> [];
'ibm-not-wf-P87-ibm87n27'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n27.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n26'(suite) -> [];
'ibm-not-wf-P87-ibm87n26'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n26.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n25'(suite) -> [];
'ibm-not-wf-P87-ibm87n25'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n25.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n24'(suite) -> [];
'ibm-not-wf-P87-ibm87n24'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n24.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n23'(suite) -> [];
'ibm-not-wf-P87-ibm87n23'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n23.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n22'(suite) -> [];
'ibm-not-wf-P87-ibm87n22'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n22.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n21'(suite) -> [];
'ibm-not-wf-P87-ibm87n21'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n21.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n20'(suite) -> [];
'ibm-not-wf-P87-ibm87n20'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n20.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n19'(suite) -> [];
'ibm-not-wf-P87-ibm87n19'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n19.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n18'(suite) -> [];
'ibm-not-wf-P87-ibm87n18'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n18.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n17'(suite) -> [];
'ibm-not-wf-P87-ibm87n17'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n17.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n16'(suite) -> [];
'ibm-not-wf-P87-ibm87n16'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n16.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n15'(suite) -> [];
'ibm-not-wf-P87-ibm87n15'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n15.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n14'(suite) -> [];
'ibm-not-wf-P87-ibm87n14'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n14.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n13'(suite) -> [];
'ibm-not-wf-P87-ibm87n13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n12'(suite) -> [];
'ibm-not-wf-P87-ibm87n12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n11'(suite) -> [];
'ibm-not-wf-P87-ibm87n11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n10'(suite) -> [];
'ibm-not-wf-P87-ibm87n10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n09'(suite) -> [];
'ibm-not-wf-P87-ibm87n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n08'(suite) -> [];
'ibm-not-wf-P87-ibm87n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n07'(suite) -> [];
'ibm-not-wf-P87-ibm87n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n06'(suite) -> [];
'ibm-not-wf-P87-ibm87n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n05'(suite) -> [];
'ibm-not-wf-P87-ibm87n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n04'(suite) -> [];
'ibm-not-wf-P87-ibm87n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n03'(suite) -> [];
'ibm-not-wf-P87-ibm87n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n02'(suite) -> [];
'ibm-not-wf-P87-ibm87n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P87-ibm87n01'(suite) -> [];
'ibm-not-wf-P87-ibm87n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P87-ibm87n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P86-ibm86n04'(suite) -> [];
'ibm-not-wf-P86-ibm86n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P86-ibm86n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P86-ibm86n03'(suite) -> [];
'ibm-not-wf-P86-ibm86n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P86-ibm86n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P86-ibm86n02'(suite) -> [];
'ibm-not-wf-P86-ibm86n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P86-ibm86n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P86-ibm86n01'(suite) -> [];
'ibm-not-wf-P86-ibm86n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P86-ibm86n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n99'(suite) -> [];
'ibm-not-wf-P85-ibm85n99'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n99.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n98'(suite) -> [];
'ibm-not-wf-P85-ibm85n98'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n98.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n97'(suite) -> [];
'ibm-not-wf-P85-ibm85n97'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n97.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n96'(suite) -> [];
'ibm-not-wf-P85-ibm85n96'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n96.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n95'(suite) -> [];
'ibm-not-wf-P85-ibm85n95'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n95.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n94'(suite) -> [];
'ibm-not-wf-P85-ibm85n94'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n94.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n93'(suite) -> [];
'ibm-not-wf-P85-ibm85n93'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n93.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n92'(suite) -> [];
'ibm-not-wf-P85-ibm85n92'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n92.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n91'(suite) -> [];
'ibm-not-wf-P85-ibm85n91'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n91.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n90'(suite) -> [];
'ibm-not-wf-P85-ibm85n90'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n90.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n89'(suite) -> [];
'ibm-not-wf-P85-ibm85n89'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n89.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n88'(suite) -> [];
'ibm-not-wf-P85-ibm85n88'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n88.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n87'(suite) -> [];
'ibm-not-wf-P85-ibm85n87'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n87.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n86'(suite) -> [];
'ibm-not-wf-P85-ibm85n86'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n86.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n85'(suite) -> [];
'ibm-not-wf-P85-ibm85n85'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n85.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n84'(suite) -> [];
'ibm-not-wf-P85-ibm85n84'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n84.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n83'(suite) -> [];
'ibm-not-wf-P85-ibm85n83'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n83.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n82'(suite) -> [];
'ibm-not-wf-P85-ibm85n82'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n82.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n81'(suite) -> [];
'ibm-not-wf-P85-ibm85n81'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n81.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n80'(suite) -> [];
'ibm-not-wf-P85-ibm85n80'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n80.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n79'(suite) -> [];
'ibm-not-wf-P85-ibm85n79'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n79.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n78'(suite) -> [];
'ibm-not-wf-P85-ibm85n78'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n78.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n77'(suite) -> [];
'ibm-not-wf-P85-ibm85n77'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n77.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n76'(suite) -> [];
'ibm-not-wf-P85-ibm85n76'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n76.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n75'(suite) -> [];
'ibm-not-wf-P85-ibm85n75'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n75.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n74'(suite) -> [];
'ibm-not-wf-P85-ibm85n74'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n74.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n73'(suite) -> [];
'ibm-not-wf-P85-ibm85n73'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n73.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n72'(suite) -> [];
'ibm-not-wf-P85-ibm85n72'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n72.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n71'(suite) -> [];
'ibm-not-wf-P85-ibm85n71'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n71.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n70'(suite) -> [];
'ibm-not-wf-P85-ibm85n70'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n70.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n69'(suite) -> [];
'ibm-not-wf-P85-ibm85n69'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n69.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n68'(suite) -> [];
'ibm-not-wf-P85-ibm85n68'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n68.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n67'(suite) -> [];
'ibm-not-wf-P85-ibm85n67'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n67.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n66'(suite) -> [];
'ibm-not-wf-P85-ibm85n66'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n66.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n65'(suite) -> [];
'ibm-not-wf-P85-ibm85n65'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n65.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n64'(suite) -> [];
'ibm-not-wf-P85-ibm85n64'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n64.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n63'(suite) -> [];
'ibm-not-wf-P85-ibm85n63'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n63.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n62'(suite) -> [];
'ibm-not-wf-P85-ibm85n62'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n62.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n61'(suite) -> [];
'ibm-not-wf-P85-ibm85n61'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n61.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n60'(suite) -> [];
'ibm-not-wf-P85-ibm85n60'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n60.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n59'(suite) -> [];
'ibm-not-wf-P85-ibm85n59'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n59.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n58'(suite) -> [];
'ibm-not-wf-P85-ibm85n58'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n58.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n57'(suite) -> [];
'ibm-not-wf-P85-ibm85n57'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n57.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n56'(suite) -> [];
'ibm-not-wf-P85-ibm85n56'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n56.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n55'(suite) -> [];
'ibm-not-wf-P85-ibm85n55'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n55.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n54'(suite) -> [];
'ibm-not-wf-P85-ibm85n54'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n54.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n53'(suite) -> [];
'ibm-not-wf-P85-ibm85n53'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n53.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n52'(suite) -> [];
'ibm-not-wf-P85-ibm85n52'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n52.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n51'(suite) -> [];
'ibm-not-wf-P85-ibm85n51'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n51.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n50'(suite) -> [];
'ibm-not-wf-P85-ibm85n50'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n50.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n49'(suite) -> [];
'ibm-not-wf-P85-ibm85n49'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n49.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n48'(suite) -> [];
'ibm-not-wf-P85-ibm85n48'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n48.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n47'(suite) -> [];
'ibm-not-wf-P85-ibm85n47'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n47.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n46'(suite) -> [];
'ibm-not-wf-P85-ibm85n46'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n46.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n45'(suite) -> [];
'ibm-not-wf-P85-ibm85n45'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n45.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n44'(suite) -> [];
'ibm-not-wf-P85-ibm85n44'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n44.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n43'(suite) -> [];
'ibm-not-wf-P85-ibm85n43'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n43.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n42'(suite) -> [];
'ibm-not-wf-P85-ibm85n42'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n42.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n41'(suite) -> [];
'ibm-not-wf-P85-ibm85n41'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n41.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n40'(suite) -> [];
'ibm-not-wf-P85-ibm85n40'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n40.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n39'(suite) -> [];
'ibm-not-wf-P85-ibm85n39'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n39.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n38'(suite) -> [];
'ibm-not-wf-P85-ibm85n38'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n38.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n37'(suite) -> [];
'ibm-not-wf-P85-ibm85n37'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n37.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n36'(suite) -> [];
'ibm-not-wf-P85-ibm85n36'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n36.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n35'(suite) -> [];
'ibm-not-wf-P85-ibm85n35'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n35.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n34'(suite) -> [];
'ibm-not-wf-P85-ibm85n34'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n34.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n33'(suite) -> [];
'ibm-not-wf-P85-ibm85n33'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n33.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n32'(suite) -> [];
'ibm-not-wf-P85-ibm85n32'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n32.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n31'(suite) -> [];
'ibm-not-wf-P85-ibm85n31'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n31.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n30'(suite) -> [];
'ibm-not-wf-P85-ibm85n30'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n30.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n29'(suite) -> [];
'ibm-not-wf-P85-ibm85n29'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n29.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n28'(suite) -> [];
'ibm-not-wf-P85-ibm85n28'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n28.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n27'(suite) -> [];
'ibm-not-wf-P85-ibm85n27'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n27.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n26'(suite) -> [];
'ibm-not-wf-P85-ibm85n26'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n26.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n25'(suite) -> [];
'ibm-not-wf-P85-ibm85n25'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n25.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n24'(suite) -> [];
'ibm-not-wf-P85-ibm85n24'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n24.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n23'(suite) -> [];
'ibm-not-wf-P85-ibm85n23'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n23.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n22'(suite) -> [];
'ibm-not-wf-P85-ibm85n22'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n22.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n21'(suite) -> [];
'ibm-not-wf-P85-ibm85n21'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n21.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n20'(suite) -> [];
'ibm-not-wf-P85-ibm85n20'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n20.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n198'(suite) -> [];
'ibm-not-wf-P85-ibm85n198'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n198.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n197'(suite) -> [];
'ibm-not-wf-P85-ibm85n197'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n197.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n196'(suite) -> [];
'ibm-not-wf-P85-ibm85n196'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n196.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n195'(suite) -> [];
'ibm-not-wf-P85-ibm85n195'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n195.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n194'(suite) -> [];
'ibm-not-wf-P85-ibm85n194'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n194.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n193'(suite) -> [];
'ibm-not-wf-P85-ibm85n193'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n193.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n192'(suite) -> [];
'ibm-not-wf-P85-ibm85n192'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n192.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n191'(suite) -> [];
'ibm-not-wf-P85-ibm85n191'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n191.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n190'(suite) -> [];
'ibm-not-wf-P85-ibm85n190'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n190.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n19'(suite) -> [];
'ibm-not-wf-P85-ibm85n19'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n19.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n189'(suite) -> [];
'ibm-not-wf-P85-ibm85n189'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n189.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n188'(suite) -> [];
'ibm-not-wf-P85-ibm85n188'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n188.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n187'(suite) -> [];
'ibm-not-wf-P85-ibm85n187'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n187.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n186'(suite) -> [];
'ibm-not-wf-P85-ibm85n186'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n186.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n185'(suite) -> [];
'ibm-not-wf-P85-ibm85n185'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n185.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n184'(suite) -> [];
'ibm-not-wf-P85-ibm85n184'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n184.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n183'(suite) -> [];
'ibm-not-wf-P85-ibm85n183'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n183.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n182'(suite) -> [];
'ibm-not-wf-P85-ibm85n182'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n182.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n181'(suite) -> [];
'ibm-not-wf-P85-ibm85n181'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n181.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n180'(suite) -> [];
'ibm-not-wf-P85-ibm85n180'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n180.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n18'(suite) -> [];
'ibm-not-wf-P85-ibm85n18'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n18.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n179'(suite) -> [];
'ibm-not-wf-P85-ibm85n179'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n179.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n178'(suite) -> [];
'ibm-not-wf-P85-ibm85n178'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n178.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n177'(suite) -> [];
'ibm-not-wf-P85-ibm85n177'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n177.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n176'(suite) -> [];
'ibm-not-wf-P85-ibm85n176'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n176.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n175'(suite) -> [];
'ibm-not-wf-P85-ibm85n175'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n175.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n174'(suite) -> [];
'ibm-not-wf-P85-ibm85n174'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n174.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n173'(suite) -> [];
'ibm-not-wf-P85-ibm85n173'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n173.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n172'(suite) -> [];
'ibm-not-wf-P85-ibm85n172'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n172.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n171'(suite) -> [];
'ibm-not-wf-P85-ibm85n171'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n171.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n170'(suite) -> [];
'ibm-not-wf-P85-ibm85n170'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n170.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n17'(suite) -> [];
'ibm-not-wf-P85-ibm85n17'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n17.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n169'(suite) -> [];
'ibm-not-wf-P85-ibm85n169'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n169.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n168'(suite) -> [];
'ibm-not-wf-P85-ibm85n168'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n168.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n167'(suite) -> [];
'ibm-not-wf-P85-ibm85n167'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n167.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n166'(suite) -> [];
'ibm-not-wf-P85-ibm85n166'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n166.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n165'(suite) -> [];
'ibm-not-wf-P85-ibm85n165'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n165.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n164'(suite) -> [];
'ibm-not-wf-P85-ibm85n164'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n164.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n163'(suite) -> [];
'ibm-not-wf-P85-ibm85n163'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n163.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n162'(suite) -> [];
'ibm-not-wf-P85-ibm85n162'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n162.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n161'(suite) -> [];
'ibm-not-wf-P85-ibm85n161'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n161.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n160'(suite) -> [];
'ibm-not-wf-P85-ibm85n160'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n160.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n16'(suite) -> [];
'ibm-not-wf-P85-ibm85n16'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n16.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n159'(suite) -> [];
'ibm-not-wf-P85-ibm85n159'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n159.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n158'(suite) -> [];
'ibm-not-wf-P85-ibm85n158'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n158.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n157'(suite) -> [];
'ibm-not-wf-P85-ibm85n157'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n157.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n156'(suite) -> [];
'ibm-not-wf-P85-ibm85n156'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n156.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n155'(suite) -> [];
'ibm-not-wf-P85-ibm85n155'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n155.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n154'(suite) -> [];
'ibm-not-wf-P85-ibm85n154'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n154.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n153'(suite) -> [];
'ibm-not-wf-P85-ibm85n153'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n153.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n152'(suite) -> [];
'ibm-not-wf-P85-ibm85n152'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n152.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n151'(suite) -> [];
'ibm-not-wf-P85-ibm85n151'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n151.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n150'(suite) -> [];
'ibm-not-wf-P85-ibm85n150'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n150.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n15'(suite) -> [];
'ibm-not-wf-P85-ibm85n15'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n15.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n149'(suite) -> [];
'ibm-not-wf-P85-ibm85n149'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n149.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n148'(suite) -> [];
'ibm-not-wf-P85-ibm85n148'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n148.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n147'(suite) -> [];
'ibm-not-wf-P85-ibm85n147'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n147.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n146'(suite) -> [];
'ibm-not-wf-P85-ibm85n146'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n146.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n145'(suite) -> [];
'ibm-not-wf-P85-ibm85n145'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n145.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n144'(suite) -> [];
'ibm-not-wf-P85-ibm85n144'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n144.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n143'(suite) -> [];
'ibm-not-wf-P85-ibm85n143'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n143.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n142'(suite) -> [];
'ibm-not-wf-P85-ibm85n142'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n142.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n141'(suite) -> [];
'ibm-not-wf-P85-ibm85n141'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n141.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n140'(suite) -> [];
'ibm-not-wf-P85-ibm85n140'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n140.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n14'(suite) -> [];
'ibm-not-wf-P85-ibm85n14'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n14.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n139'(suite) -> [];
'ibm-not-wf-P85-ibm85n139'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n139.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n138'(suite) -> [];
'ibm-not-wf-P85-ibm85n138'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n138.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n137'(suite) -> [];
'ibm-not-wf-P85-ibm85n137'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n137.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n136'(suite) -> [];
'ibm-not-wf-P85-ibm85n136'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n136.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n135'(suite) -> [];
'ibm-not-wf-P85-ibm85n135'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n135.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n134'(suite) -> [];
'ibm-not-wf-P85-ibm85n134'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n134.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n133'(suite) -> [];
'ibm-not-wf-P85-ibm85n133'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n133.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n132'(suite) -> [];
'ibm-not-wf-P85-ibm85n132'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n132.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n131'(suite) -> [];
'ibm-not-wf-P85-ibm85n131'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n131.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n130'(suite) -> [];
'ibm-not-wf-P85-ibm85n130'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n130.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n13'(suite) -> [];
'ibm-not-wf-P85-ibm85n13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n129'(suite) -> [];
'ibm-not-wf-P85-ibm85n129'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n129.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n128'(suite) -> [];
'ibm-not-wf-P85-ibm85n128'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n128.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n127'(suite) -> [];
'ibm-not-wf-P85-ibm85n127'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n127.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n126'(suite) -> [];
'ibm-not-wf-P85-ibm85n126'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n126.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n125'(suite) -> [];
'ibm-not-wf-P85-ibm85n125'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n125.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n124'(suite) -> [];
'ibm-not-wf-P85-ibm85n124'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n124.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n123'(suite) -> [];
'ibm-not-wf-P85-ibm85n123'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n123.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n122'(suite) -> [];
'ibm-not-wf-P85-ibm85n122'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n122.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n121'(suite) -> [];
'ibm-not-wf-P85-ibm85n121'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n121.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n120'(suite) -> [];
'ibm-not-wf-P85-ibm85n120'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n120.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n12'(suite) -> [];
'ibm-not-wf-P85-ibm85n12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n119'(suite) -> [];
'ibm-not-wf-P85-ibm85n119'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n119.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n118'(suite) -> [];
'ibm-not-wf-P85-ibm85n118'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n118.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n117'(suite) -> [];
'ibm-not-wf-P85-ibm85n117'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n117.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n116'(suite) -> [];
'ibm-not-wf-P85-ibm85n116'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n116.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n115'(suite) -> [];
'ibm-not-wf-P85-ibm85n115'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n115.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n114'(suite) -> [];
'ibm-not-wf-P85-ibm85n114'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n114.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n113'(suite) -> [];
'ibm-not-wf-P85-ibm85n113'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n113.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n112'(suite) -> [];
'ibm-not-wf-P85-ibm85n112'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n112.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n111'(suite) -> [];
'ibm-not-wf-P85-ibm85n111'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n111.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n110'(suite) -> [];
'ibm-not-wf-P85-ibm85n110'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n110.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n11'(suite) -> [];
'ibm-not-wf-P85-ibm85n11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n109'(suite) -> [];
'ibm-not-wf-P85-ibm85n109'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n109.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n108'(suite) -> [];
'ibm-not-wf-P85-ibm85n108'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n108.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n107'(suite) -> [];
'ibm-not-wf-P85-ibm85n107'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n107.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n106'(suite) -> [];
'ibm-not-wf-P85-ibm85n106'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n106.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n105'(suite) -> [];
'ibm-not-wf-P85-ibm85n105'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n105.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n104'(suite) -> [];
'ibm-not-wf-P85-ibm85n104'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n104.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n103'(suite) -> [];
'ibm-not-wf-P85-ibm85n103'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n103.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n102'(suite) -> [];
'ibm-not-wf-P85-ibm85n102'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n102.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n101'(suite) -> [];
'ibm-not-wf-P85-ibm85n101'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n101.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n100'(suite) -> [];
'ibm-not-wf-P85-ibm85n100'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n100.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n10'(suite) -> [];
'ibm-not-wf-P85-ibm85n10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n09'(suite) -> [];
'ibm-not-wf-P85-ibm85n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n08'(suite) -> [];
'ibm-not-wf-P85-ibm85n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n07'(suite) -> [];
'ibm-not-wf-P85-ibm85n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n06'(suite) -> [];
'ibm-not-wf-P85-ibm85n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n05'(suite) -> [];
'ibm-not-wf-P85-ibm85n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n04'(suite) -> [];
'ibm-not-wf-P85-ibm85n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n03'(suite) -> [];
'ibm-not-wf-P85-ibm85n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n02'(suite) -> [];
'ibm-not-wf-P85-ibm85n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P85-ibm85n01'(suite) -> [];
'ibm-not-wf-P85-ibm85n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P85-ibm85n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P83-ibm83n06'(suite) -> [];
'ibm-not-wf-P83-ibm83n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P83-ibm83n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P83-ibm83n05'(suite) -> [];
'ibm-not-wf-P83-ibm83n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P83-ibm83n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P83-ibm83n04'(suite) -> [];
'ibm-not-wf-P83-ibm83n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P83-ibm83n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P83-ibm83n03'(suite) -> [];
'ibm-not-wf-P83-ibm83n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P83-ibm83n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P83-ibm83n02'(suite) -> [];
'ibm-not-wf-P83-ibm83n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P83-ibm83n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P83-ibm83n01'(suite) -> [];
'ibm-not-wf-P83-ibm83n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P83-ibm83n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P82-ibm82n08'(suite) -> [];
'ibm-not-wf-P82-ibm82n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P82-ibm82n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P82-ibm82n07'(suite) -> [];
'ibm-not-wf-P82-ibm82n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P82-ibm82n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P82-ibm82n06'(suite) -> [];
'ibm-not-wf-P82-ibm82n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P82-ibm82n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P82-ibm82n05'(suite) -> [];
'ibm-not-wf-P82-ibm82n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P82-ibm82n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P82-ibm82n04'(suite) -> [];
'ibm-not-wf-P82-ibm82n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P82-ibm82n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P82-ibm82n03'(suite) -> [];
'ibm-not-wf-P82-ibm82n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P82-ibm82n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P82-ibm82n02'(suite) -> [];
'ibm-not-wf-P82-ibm82n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P82-ibm82n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P82-ibm82n01'(suite) -> [];
'ibm-not-wf-P82-ibm82n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P82-ibm82n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P81-ibm81n09'(suite) -> [];
'ibm-not-wf-P81-ibm81n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P81-ibm81n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P81-ibm81n08'(suite) -> [];
'ibm-not-wf-P81-ibm81n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P81-ibm81n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P81-ibm81n07'(suite) -> [];
'ibm-not-wf-P81-ibm81n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P81-ibm81n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P81-ibm81n06'(suite) -> [];
'ibm-not-wf-P81-ibm81n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P81-ibm81n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P81-ibm81n05'(suite) -> [];
'ibm-not-wf-P81-ibm81n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P81-ibm81n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P81-ibm81n04'(suite) -> [];
'ibm-not-wf-P81-ibm81n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P81-ibm81n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P81-ibm81n03'(suite) -> [];
'ibm-not-wf-P81-ibm81n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P81-ibm81n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P81-ibm81n02'(suite) -> [];
'ibm-not-wf-P81-ibm81n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P81-ibm81n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P81-ibm81n01'(suite) -> [];
'ibm-not-wf-P81-ibm81n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P81-ibm81n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P80-ibm80n06'(suite) -> [];
'ibm-not-wf-P80-ibm80n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P80-ibm80n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P80-ibm80n05'(suite) -> [];
'ibm-not-wf-P80-ibm80n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P80-ibm80n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P80-ibm80n04'(suite) -> [];
'ibm-not-wf-P80-ibm80n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P80-ibm80n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P80-ibm80n03'(suite) -> [];
'ibm-not-wf-P80-ibm80n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P80-ibm80n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P80-ibm80n02'(suite) -> [];
'ibm-not-wf-P80-ibm80n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P80-ibm80n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P80-ibm80n01'(suite) -> [];
'ibm-not-wf-P80-ibm80n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P80-ibm80n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P79-ibm79n02'(suite) -> [];
'ibm-not-wf-P79-ibm79n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P79-ibm79n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P79-ibm79n01'(suite) -> [];
'ibm-not-wf-P79-ibm79n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P79-ibm79n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P78-ibm78n02'(suite) -> [];
'ibm-not-wf-P78-ibm78n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P78-ibm78n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P78-ibm78n01'(suite) -> [];
'ibm-not-wf-P78-ibm78n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P78-ibm78n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P77-ibm77n04'(suite) -> [];
'ibm-not-wf-P77-ibm77n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P77-ibm77n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P77-ibm77n03'(suite) -> [];
'ibm-not-wf-P77-ibm77n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P77-ibm77n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P77-ibm77n02'(suite) -> [];
'ibm-not-wf-P77-ibm77n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P77-ibm77n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P77-ibm77n01'(suite) -> [];
'ibm-not-wf-P77-ibm77n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P77-ibm77n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P76-ibm76n07'(suite) -> [];
'ibm-not-wf-P76-ibm76n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P76-ibm76n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P76-ibm76n06'(suite) -> [];
'ibm-not-wf-P76-ibm76n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P76-ibm76n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P76-ibm76n05'(suite) -> [];
'ibm-not-wf-P76-ibm76n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P76-ibm76n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P76-ibm76n04'(suite) -> [];
'ibm-not-wf-P76-ibm76n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P76-ibm76n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P76-ibm76n03'(suite) -> [];
'ibm-not-wf-P76-ibm76n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P76-ibm76n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P76-ibm76n02'(suite) -> [];
'ibm-not-wf-P76-ibm76n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P76-ibm76n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P76-ibm76n01'(suite) -> [];
'ibm-not-wf-P76-ibm76n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P76-ibm76n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P75-ibm75n13'(suite) -> [];
'ibm-not-wf-P75-ibm75n13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P75-ibm75n13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P75-ibm75n12'(suite) -> [];
'ibm-not-wf-P75-ibm75n12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P75-ibm75n12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P75-ibm75n11'(suite) -> [];
'ibm-not-wf-P75-ibm75n11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P75-ibm75n11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P75-ibm75n10'(suite) -> [];
'ibm-not-wf-P75-ibm75n10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P75-ibm75n10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P75-ibm75n09'(suite) -> [];
'ibm-not-wf-P75-ibm75n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P75-ibm75n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P75-ibm75n08'(suite) -> [];
'ibm-not-wf-P75-ibm75n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P75-ibm75n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P75-ibm75n07'(suite) -> [];
'ibm-not-wf-P75-ibm75n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P75-ibm75n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P75-ibm75n06'(suite) -> [];
'ibm-not-wf-P75-ibm75n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P75-ibm75n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P75-ibm75n05'(suite) -> [];
'ibm-not-wf-P75-ibm75n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P75-ibm75n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P75-ibm75n04'(suite) -> [];
'ibm-not-wf-P75-ibm75n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P75-ibm75n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P75-ibm75n03'(suite) -> [];
'ibm-not-wf-P75-ibm75n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P75-ibm75n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P75-ibm75n02'(suite) -> [];
'ibm-not-wf-P75-ibm75n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P75-ibm75n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P75-ibm75n01'(suite) -> [];
'ibm-not-wf-P75-ibm75n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P75-ibm75n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P74-ibm74n01'(suite) -> [];
'ibm-not-wf-P74-ibm74n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P74-ibm74n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P73-ibm73n03'(suite) -> [];
'ibm-not-wf-P73-ibm73n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P73-ibm73n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P73-ibm73n01'(suite) -> [];
'ibm-not-wf-P73-ibm73n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P73-ibm73n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P72-ibm72n09'(suite) -> [];
'ibm-not-wf-P72-ibm72n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P72-ibm72n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P72-ibm72n08'(suite) -> [];
'ibm-not-wf-P72-ibm72n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P72-ibm72n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P72-ibm72n07'(suite) -> [];
'ibm-not-wf-P72-ibm72n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P72-ibm72n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P72-ibm72n06'(suite) -> [];
'ibm-not-wf-P72-ibm72n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P72-ibm72n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P72-ibm72n05'(suite) -> [];
'ibm-not-wf-P72-ibm72n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P72-ibm72n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P72-ibm72n04'(suite) -> [];
'ibm-not-wf-P72-ibm72n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P72-ibm72n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P72-ibm72n03'(suite) -> [];
'ibm-not-wf-P72-ibm72n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P72-ibm72n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P72-ibm72n02'(suite) -> [];
'ibm-not-wf-P72-ibm72n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P72-ibm72n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P72-ibm72n01'(suite) -> [];
'ibm-not-wf-P72-ibm72n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P72-ibm72n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P71-ibm71n08'(suite) -> [];
'ibm-not-wf-P71-ibm71n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P71-ibm71n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P71-ibm71n07'(suite) -> [];
'ibm-not-wf-P71-ibm71n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P71-ibm71n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P71-ibm71n06'(suite) -> [];
'ibm-not-wf-P71-ibm71n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P71-ibm71n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P71-ibm71n05'(suite) -> [];
'ibm-not-wf-P71-ibm71n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P71-ibm71n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P71-ibm71n04'(suite) -> [];
'ibm-not-wf-P71-ibm71n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P71-ibm71n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P71-ibm71n03'(suite) -> [];
'ibm-not-wf-P71-ibm71n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P71-ibm71n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P71-ibm71n02'(suite) -> [];
'ibm-not-wf-P71-ibm71n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P71-ibm71n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P71-ibm71n01'(suite) -> [];
'ibm-not-wf-P71-ibm71n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P71-ibm71n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P71-ibm70n01'(suite) -> [];
'ibm-not-wf-P71-ibm70n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P71-ibm70n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P69-ibm69n07'(suite) -> [];
'ibm-not-wf-P69-ibm69n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P69-ibm69n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P69-ibm69n06'(suite) -> [];
'ibm-not-wf-P69-ibm69n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P69-ibm69n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P69-ibm69n05'(suite) -> [];
'ibm-not-wf-P69-ibm69n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P69-ibm69n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P69-ibm69n04'(suite) -> [];
'ibm-not-wf-P69-ibm69n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P69-ibm69n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P69-ibm69n03'(suite) -> [];
'ibm-not-wf-P69-ibm69n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P69-ibm69n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P69-ibm69n02'(suite) -> [];
'ibm-not-wf-P69-ibm69n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P69-ibm69n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P69-ibm69n01'(suite) -> [];
'ibm-not-wf-P69-ibm69n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P69-ibm69n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P68-ibm68n10'(suite) -> [];
'ibm-not-wf-P68-ibm68n10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P68-ibm68n10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P68-ibm68n09'(suite) -> [];
'ibm-not-wf-P68-ibm68n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P68-ibm68n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P68-ibm68n08'(suite) -> [];
'ibm-not-wf-P68-ibm68n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P68-ibm68n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P68-ibm68n07'(suite) -> [];
'ibm-not-wf-P68-ibm68n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P68-ibm68n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P68-ibm68n06'(suite) -> [];
'ibm-not-wf-P68-ibm68n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P68-ibm68n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P68-ibm68n05'(suite) -> [];
'ibm-not-wf-P68-ibm68n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P68-ibm68n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P68-ibm68n04'(suite) -> [];
'ibm-not-wf-P68-ibm68n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P68-ibm68n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P68-ibm68n03'(suite) -> [];
'ibm-not-wf-P68-ibm68n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P68-ibm68n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P68-ibm68n02'(suite) -> [];
'ibm-not-wf-P68-ibm68n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P68-ibm68n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P68-ibm68n01'(suite) -> [];
'ibm-not-wf-P68-ibm68n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P68-ibm68n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n15'(suite) -> [];
'ibm-not-wf-P66-ibm66n15'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n15.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n14'(suite) -> [];
'ibm-not-wf-P66-ibm66n14'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n14.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n13'(suite) -> [];
'ibm-not-wf-P66-ibm66n13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n12'(suite) -> [];
'ibm-not-wf-P66-ibm66n12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n11'(suite) -> [];
'ibm-not-wf-P66-ibm66n11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n10'(suite) -> [];
'ibm-not-wf-P66-ibm66n10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n09'(suite) -> [];
'ibm-not-wf-P66-ibm66n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n08'(suite) -> [];
'ibm-not-wf-P66-ibm66n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n07'(suite) -> [];
'ibm-not-wf-P66-ibm66n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n06'(suite) -> [];
'ibm-not-wf-P66-ibm66n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n05'(suite) -> [];
'ibm-not-wf-P66-ibm66n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n04'(suite) -> [];
'ibm-not-wf-P66-ibm66n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n03'(suite) -> [];
'ibm-not-wf-P66-ibm66n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n02'(suite) -> [];
'ibm-not-wf-P66-ibm66n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P66-ibm66n01'(suite) -> [];
'ibm-not-wf-P66-ibm66n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P66-ibm66n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P65-ibm65n02'(suite) -> [];
'ibm-not-wf-P65-ibm65n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P65-ibm65n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P65-ibm65n01'(suite) -> [];
'ibm-not-wf-P65-ibm65n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P65-ibm65n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P64-ibm64n03'(suite) -> [];
'ibm-not-wf-P64-ibm64n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P64-ibm64n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P64-ibm64n02'(suite) -> [];
'ibm-not-wf-P64-ibm64n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P64-ibm64n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P64-ibm64n01'(suite) -> [];
'ibm-not-wf-P64-ibm64n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P64-ibm64n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P63-ibm63n07'(suite) -> [];
'ibm-not-wf-P63-ibm63n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P63-ibm63n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P63-ibm63n06'(suite) -> [];
'ibm-not-wf-P63-ibm63n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P63-ibm63n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P63-ibm63n05'(suite) -> [];
'ibm-not-wf-P63-ibm63n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P63-ibm63n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P63-ibm63n04'(suite) -> [];
'ibm-not-wf-P63-ibm63n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P63-ibm63n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P63-ibm63n03'(suite) -> [];
'ibm-not-wf-P63-ibm63n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P63-ibm63n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P63-ibm63n02'(suite) -> [];
'ibm-not-wf-P63-ibm63n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P63-ibm63n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P63-ibm63n01'(suite) -> [];
'ibm-not-wf-P63-ibm63n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P63-ibm63n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P62-ibm62n08'(suite) -> [];
'ibm-not-wf-P62-ibm62n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P62-ibm62n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P62-ibm62n07'(suite) -> [];
'ibm-not-wf-P62-ibm62n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P62-ibm62n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P62-ibm62n06'(suite) -> [];
'ibm-not-wf-P62-ibm62n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P62-ibm62n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P62-ibm62n05'(suite) -> [];
'ibm-not-wf-P62-ibm62n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P62-ibm62n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P62-ibm62n04'(suite) -> [];
'ibm-not-wf-P62-ibm62n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P62-ibm62n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P62-ibm62n03'(suite) -> [];
'ibm-not-wf-P62-ibm62n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P62-ibm62n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P62-ibm62n02'(suite) -> [];
'ibm-not-wf-P62-ibm62n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P62-ibm62n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P62-ibm62n01'(suite) -> [];
'ibm-not-wf-P62-ibm62n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P62-ibm62n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P61-ibm61n01'(suite) -> [];
'ibm-not-wf-P61-ibm61n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P61-ibm61n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P60-ibm60n08'(suite) -> [];
'ibm-not-wf-P60-ibm60n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P60-ibm60n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P60-ibm60n07'(suite) -> [];
'ibm-not-wf-P60-ibm60n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P60-ibm60n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P60-ibm60n06'(suite) -> [];
'ibm-not-wf-P60-ibm60n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P60-ibm60n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P60-ibm60n05'(suite) -> [];
'ibm-not-wf-P60-ibm60n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P60-ibm60n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P60-ibm60n04'(suite) -> [];
'ibm-not-wf-P60-ibm60n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P60-ibm60n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P60-ibm60n03'(suite) -> [];
'ibm-not-wf-P60-ibm60n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P60-ibm60n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P60-ibm60n02'(suite) -> [];
'ibm-not-wf-P60-ibm60n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P60-ibm60n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P60-ibm60n01'(suite) -> [];
'ibm-not-wf-P60-ibm60n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P60-ibm60n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P59-ibm59n06'(suite) -> [];
'ibm-not-wf-P59-ibm59n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P59-ibm59n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P59-ibm59n05'(suite) -> [];
'ibm-not-wf-P59-ibm59n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P59-ibm59n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P59-ibm59n04'(suite) -> [];
'ibm-not-wf-P59-ibm59n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P59-ibm59n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P59-ibm59n03'(suite) -> [];
'ibm-not-wf-P59-ibm59n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P59-ibm59n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P59-ibm59n02'(suite) -> [];
'ibm-not-wf-P59-ibm59n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P59-ibm59n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P59-ibm59n01'(suite) -> [];
'ibm-not-wf-P59-ibm59n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P59-ibm59n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P58-ibm58n08'(suite) -> [];
'ibm-not-wf-P58-ibm58n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P58-ibm58n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P58-ibm58n07'(suite) -> [];
'ibm-not-wf-P58-ibm58n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P58-ibm58n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P58-ibm58n06'(suite) -> [];
'ibm-not-wf-P58-ibm58n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P58-ibm58n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P58-ibm58n05'(suite) -> [];
'ibm-not-wf-P58-ibm58n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P58-ibm58n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P58-ibm58n04'(suite) -> [];
'ibm-not-wf-P58-ibm58n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P58-ibm58n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P58-ibm58n03'(suite) -> [];
'ibm-not-wf-P58-ibm58n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P58-ibm58n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P58-ibm58n02'(suite) -> [];
'ibm-not-wf-P58-ibm58n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P58-ibm58n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P58-ibm58n01'(suite) -> [];
'ibm-not-wf-P58-ibm58n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P58-ibm58n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P57-ibm57n01'(suite) -> [];
'ibm-not-wf-P57-ibm57n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P57-ibm57n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P56-ibm56n07'(suite) -> [];
'ibm-not-wf-P56-ibm56n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P56-ibm56n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P56-ibm56n06'(suite) -> [];
'ibm-not-wf-P56-ibm56n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P56-ibm56n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P56-ibm56n05'(suite) -> [];
'ibm-not-wf-P56-ibm56n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P56-ibm56n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P56-ibm56n04'(suite) -> [];
'ibm-not-wf-P56-ibm56n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P56-ibm56n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P56-ibm56n03'(suite) -> [];
'ibm-not-wf-P56-ibm56n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P56-ibm56n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P56-ibm56n02'(suite) -> [];
'ibm-not-wf-P56-ibm56n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P56-ibm56n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P56-ibm56n01'(suite) -> [];
'ibm-not-wf-P56-ibm56n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P56-ibm56n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P55-ibm55n03'(suite) -> [];
'ibm-not-wf-P55-ibm55n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P55-ibm55n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P55-ibm55n02'(suite) -> [];
'ibm-not-wf-P55-ibm55n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P55-ibm55n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P55-ibm55n01'(suite) -> [];
'ibm-not-wf-P55-ibm55n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P55-ibm55n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P54-ibm54n02'(suite) -> [];
'ibm-not-wf-P54-ibm54n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P54-ibm54n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P54-ibm54n01'(suite) -> [];
'ibm-not-wf-P54-ibm54n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P54-ibm54n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P53-ibm53n08'(suite) -> [];
'ibm-not-wf-P53-ibm53n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P53-ibm53n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P53-ibm53n07'(suite) -> [];
'ibm-not-wf-P53-ibm53n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P53-ibm53n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P53-ibm53n06'(suite) -> [];
'ibm-not-wf-P53-ibm53n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P53-ibm53n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P53-ibm53n05'(suite) -> [];
'ibm-not-wf-P53-ibm53n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P53-ibm53n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P53-ibm53n04'(suite) -> [];
'ibm-not-wf-P53-ibm53n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P53-ibm53n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P53-ibm53n03'(suite) -> [];
'ibm-not-wf-P53-ibm53n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P53-ibm53n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P53-ibm53n02'(suite) -> [];
'ibm-not-wf-P53-ibm53n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P53-ibm53n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P53-ibm53n01'(suite) -> [];
'ibm-not-wf-P53-ibm53n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P53-ibm53n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P52-ibm52n06'(suite) -> [];
'ibm-not-wf-P52-ibm52n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P52-ibm52n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P52-ibm52n05'(suite) -> [];
'ibm-not-wf-P52-ibm52n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P52-ibm52n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P52-ibm52n04'(suite) -> [];
'ibm-not-wf-P52-ibm52n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P52-ibm52n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P52-ibm52n03'(suite) -> [];
'ibm-not-wf-P52-ibm52n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P52-ibm52n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P52-ibm52n02'(suite) -> [];
'ibm-not-wf-P52-ibm52n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P52-ibm52n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P52-ibm52n01'(suite) -> [];
'ibm-not-wf-P52-ibm52n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P52-ibm52n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P51-ibm51n07'(suite) -> [];
'ibm-not-wf-P51-ibm51n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P51-ibm51n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P51-ibm51n06'(suite) -> [];
'ibm-not-wf-P51-ibm51n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P51-ibm51n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P51-ibm51n05'(suite) -> [];
'ibm-not-wf-P51-ibm51n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P51-ibm51n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P51-ibm51n04'(suite) -> [];
'ibm-not-wf-P51-ibm51n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P51-ibm51n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P51-ibm51n03'(suite) -> [];
'ibm-not-wf-P51-ibm51n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P51-ibm51n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P51-ibm51n02'(suite) -> [];
'ibm-not-wf-P51-ibm51n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P51-ibm51n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P51-ibm51n01'(suite) -> [];
'ibm-not-wf-P51-ibm51n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P51-ibm51n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P50-ibm50n07'(suite) -> [];
'ibm-not-wf-P50-ibm50n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P50-ibm50n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P50-ibm50n06'(suite) -> [];
'ibm-not-wf-P50-ibm50n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P50-ibm50n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P50-ibm50n05'(suite) -> [];
'ibm-not-wf-P50-ibm50n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P50-ibm50n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P50-ibm50n04'(suite) -> [];
'ibm-not-wf-P50-ibm50n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P50-ibm50n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P50-ibm50n03'(suite) -> [];
'ibm-not-wf-P50-ibm50n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P50-ibm50n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P50-ibm50n02'(suite) -> [];
'ibm-not-wf-P50-ibm50n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P50-ibm50n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P50-ibm50n01'(suite) -> [];
'ibm-not-wf-P50-ibm50n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P50-ibm50n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P49-ibm49n06'(suite) -> [];
'ibm-not-wf-P49-ibm49n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P49-ibm49n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P49-ibm49n05'(suite) -> [];
'ibm-not-wf-P49-ibm49n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P49-ibm49n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P49-ibm49n04'(suite) -> [];
'ibm-not-wf-P49-ibm49n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P49-ibm49n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P49-ibm49n03'(suite) -> [];
'ibm-not-wf-P49-ibm49n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P49-ibm49n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P49-ibm49n02'(suite) -> [];
'ibm-not-wf-P49-ibm49n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P49-ibm49n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P49-ibm49n01'(suite) -> [];
'ibm-not-wf-P49-ibm49n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P49-ibm49n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P48-ibm48n07'(suite) -> [];
'ibm-not-wf-P48-ibm48n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P48-ibm48n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P48-ibm48n06'(suite) -> [];
'ibm-not-wf-P48-ibm48n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P48-ibm48n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P48-ibm48n05'(suite) -> [];
'ibm-not-wf-P48-ibm48n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P48-ibm48n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P48-ibm48n04'(suite) -> [];
'ibm-not-wf-P48-ibm48n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P48-ibm48n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P48-ibm48n03'(suite) -> [];
'ibm-not-wf-P48-ibm48n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P48-ibm48n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P48-ibm48n02'(suite) -> [];
'ibm-not-wf-P48-ibm48n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P48-ibm48n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P48-ibm48n01'(suite) -> [];
'ibm-not-wf-P48-ibm48n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P48-ibm48n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P47-ibm47n06'(suite) -> [];
'ibm-not-wf-P47-ibm47n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P47-ibm47n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P47-ibm47n05'(suite) -> [];
'ibm-not-wf-P47-ibm47n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P47-ibm47n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P47-ibm47n04'(suite) -> [];
'ibm-not-wf-P47-ibm47n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P47-ibm47n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P47-ibm47n03'(suite) -> [];
'ibm-not-wf-P47-ibm47n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P47-ibm47n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P47-ibm47n02'(suite) -> [];
'ibm-not-wf-P47-ibm47n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P47-ibm47n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P47-ibm47n01'(suite) -> [];
'ibm-not-wf-P47-ibm47n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P47-ibm47n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P46-ibm46n05'(suite) -> [];
'ibm-not-wf-P46-ibm46n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P46-ibm46n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P46-ibm46n04'(suite) -> [];
'ibm-not-wf-P46-ibm46n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P46-ibm46n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P46-ibm46n03'(suite) -> [];
'ibm-not-wf-P46-ibm46n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P46-ibm46n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P46-ibm46n02'(suite) -> [];
'ibm-not-wf-P46-ibm46n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P46-ibm46n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P46-ibm46n01'(suite) -> [];
'ibm-not-wf-P46-ibm46n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P46-ibm46n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P45-ibm45n09'(suite) -> [];
'ibm-not-wf-P45-ibm45n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P45-ibm45n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P45-ibm45n08'(suite) -> [];
'ibm-not-wf-P45-ibm45n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P45-ibm45n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P45-ibm45n07'(suite) -> [];
'ibm-not-wf-P45-ibm45n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P45-ibm45n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P45-ibm45n06'(suite) -> [];
'ibm-not-wf-P45-ibm45n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P45-ibm45n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P45-ibm45n05'(suite) -> [];
'ibm-not-wf-P45-ibm45n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P45-ibm45n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P45-ibm45n04'(suite) -> [];
'ibm-not-wf-P45-ibm45n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P45-ibm45n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P45-ibm45n03'(suite) -> [];
'ibm-not-wf-P45-ibm45n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P45-ibm45n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P45-ibm45n02'(suite) -> [];
'ibm-not-wf-P45-ibm45n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P45-ibm45n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P45-ibm45n01'(suite) -> [];
'ibm-not-wf-P45-ibm45n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P45-ibm45n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P44-ibm44n04'(suite) -> [];
'ibm-not-wf-P44-ibm44n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P44-ibm44n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P44-ibm44n03'(suite) -> [];
'ibm-not-wf-P44-ibm44n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P44-ibm44n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P44-ibm44n02'(suite) -> [];
'ibm-not-wf-P44-ibm44n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P44-ibm44n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P44-ibm44n01'(suite) -> [];
'ibm-not-wf-P44-ibm44n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P44-ibm44n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P43-ibm43n05'(suite) -> [];
'ibm-not-wf-P43-ibm43n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P43-ibm43n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P43-ibm43n04'(suite) -> [];
'ibm-not-wf-P43-ibm43n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P43-ibm43n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P43-ibm43n02'(suite) -> [];
'ibm-not-wf-P43-ibm43n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P43-ibm43n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P43-ibm43n01'(suite) -> [];
'ibm-not-wf-P43-ibm43n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P43-ibm43n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P42-ibm42n05'(suite) -> [];
'ibm-not-wf-P42-ibm42n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P42-ibm42n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P42-ibm42n04'(suite) -> [];
'ibm-not-wf-P42-ibm42n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P42-ibm42n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P42-ibm42n03'(suite) -> [];
'ibm-not-wf-P42-ibm42n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P42-ibm42n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P42-ibm42n02'(suite) -> [];
'ibm-not-wf-P42-ibm42n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P42-ibm42n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P42-ibm42n01'(suite) -> [];
'ibm-not-wf-P42-ibm42n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P42-ibm42n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n14'(suite) -> [];
'ibm-not-wf-P41-ibm41n14'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n14.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n13'(suite) -> [];
'ibm-not-wf-P41-ibm41n13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n12'(suite) -> [];
'ibm-not-wf-P41-ibm41n12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n11'(suite) -> [];
'ibm-not-wf-P41-ibm41n11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n10'(suite) -> [];
'ibm-not-wf-P41-ibm41n10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n09'(suite) -> [];
'ibm-not-wf-P41-ibm41n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n08'(suite) -> [];
'ibm-not-wf-P41-ibm41n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n07'(suite) -> [];
'ibm-not-wf-P41-ibm41n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n06'(suite) -> [];
'ibm-not-wf-P41-ibm41n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n05'(suite) -> [];
'ibm-not-wf-P41-ibm41n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n04'(suite) -> [];
'ibm-not-wf-P41-ibm41n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n03'(suite) -> [];
'ibm-not-wf-P41-ibm41n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n02'(suite) -> [];
'ibm-not-wf-P41-ibm41n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P41-ibm41n01'(suite) -> [];
'ibm-not-wf-P41-ibm41n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P41-ibm41n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P40-ibm40n05'(suite) -> [];
'ibm-not-wf-P40-ibm40n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P40-ibm40n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P40-ibm40n04'(suite) -> [];
'ibm-not-wf-P40-ibm40n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P40-ibm40n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P40-ibm40n03'(suite) -> [];
'ibm-not-wf-P40-ibm40n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P40-ibm40n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P40-ibm40n02'(suite) -> [];
'ibm-not-wf-P40-ibm40n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P40-ibm40n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P40-ibm40n01'(suite) -> [];
'ibm-not-wf-P40-ibm40n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P40-ibm40n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P39-ibm39n06'(suite) -> [];
'ibm-not-wf-P39-ibm39n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P39-ibm39n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P39-ibm39n05'(suite) -> [];
'ibm-not-wf-P39-ibm39n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P39-ibm39n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P39-ibm39n04'(suite) -> [];
'ibm-not-wf-P39-ibm39n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P39-ibm39n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P39-ibm39n03'(suite) -> [];
'ibm-not-wf-P39-ibm39n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P39-ibm39n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P39-ibm39n02'(suite) -> [];
'ibm-not-wf-P39-ibm39n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P39-ibm39n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P39-ibm39n01'(suite) -> [];
'ibm-not-wf-P39-ibm39n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P39-ibm39n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P32-ibm32n09'(suite) -> [];
'ibm-not-wf-P32-ibm32n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P32-ibm32n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P32-ibm32n08'(suite) -> [];
'ibm-not-wf-P32-ibm32n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P32-ibm32n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P32-ibm32n07'(suite) -> [];
'ibm-not-wf-P32-ibm32n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P32-ibm32n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P32-ibm32n06'(suite) -> [];
'ibm-not-wf-P32-ibm32n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P32-ibm32n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P32-ibm32n05'(suite) -> [];
'ibm-not-wf-P32-ibm32n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P32-ibm32n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P32-ibm32n04'(suite) -> [];
'ibm-not-wf-P32-ibm32n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P32-ibm32n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P32-ibm32n03'(suite) -> [];
'ibm-not-wf-P32-ibm32n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P32-ibm32n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P32-ibm32n02'(suite) -> [];
'ibm-not-wf-P32-ibm32n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P32-ibm32n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P32-ibm32n01'(suite) -> [];
'ibm-not-wf-P32-ibm32n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P32-ibm32n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P31-ibm31n01'(suite) -> [];
'ibm-not-wf-P31-ibm31n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P31-ibm31n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P30-ibm30n01'(suite) -> [];
'ibm-not-wf-P30-ibm30n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P30-ibm30n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P29-ibm29n07'(suite) -> [];
'ibm-not-wf-P29-ibm29n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P29-ibm29n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P29-ibm29n06'(suite) -> [];
'ibm-not-wf-P29-ibm29n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P29-ibm29n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P29-ibm29n05'(suite) -> [];
'ibm-not-wf-P29-ibm29n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P29-ibm29n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P29-ibm29n04'(suite) -> [];
'ibm-not-wf-P29-ibm29n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P29-ibm29n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P29-ibm29n03'(suite) -> [];
'ibm-not-wf-P29-ibm29n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P29-ibm29n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P29-ibm29n02'(suite) -> [];
'ibm-not-wf-P29-ibm29n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P29-ibm29n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P29-ibm29n01'(suite) -> [];
'ibm-not-wf-P29-ibm29n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P29-ibm29n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P28a-ibm28an01'(suite) -> [];
'ibm-not-wf-P28a-ibm28an01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P28a-ibm28an01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P28-ibm28n08'(suite) -> [];
'ibm-not-wf-P28-ibm28n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P28-ibm28n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P28-ibm28n07'(suite) -> [];
'ibm-not-wf-P28-ibm28n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P28-ibm28n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P28-ibm28n06'(suite) -> [];
'ibm-not-wf-P28-ibm28n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P28-ibm28n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P28-ibm28n05'(suite) -> [];
'ibm-not-wf-P28-ibm28n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P28-ibm28n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P28-ibm28n04'(suite) -> [];
'ibm-not-wf-P28-ibm28n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P28-ibm28n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P28-ibm28n03'(suite) -> [];
'ibm-not-wf-P28-ibm28n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P28-ibm28n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P28-ibm28n02'(suite) -> [];
'ibm-not-wf-P28-ibm28n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P28-ibm28n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P28-ibm28n01'(suite) -> [];
'ibm-not-wf-P28-ibm28n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P28-ibm28n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P27-ibm27n01'(suite) -> [];
'ibm-not-wf-P27-ibm27n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P27-ibm27n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P26-ibm26n01'(suite) -> [];
'ibm-not-wf-P26-ibm26n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P26-ibm26n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P25-ibm25n02'(suite) -> [];
'ibm-not-wf-P25-ibm25n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P25-ibm25n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P25-ibm25n01'(suite) -> [];
'ibm-not-wf-P25-ibm25n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P25-ibm25n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P24-ibm24n09'(suite) -> [];
'ibm-not-wf-P24-ibm24n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P24-ibm24n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P24-ibm24n08'(suite) -> [];
'ibm-not-wf-P24-ibm24n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P24-ibm24n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P24-ibm24n07'(suite) -> [];
'ibm-not-wf-P24-ibm24n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P24-ibm24n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P24-ibm24n06'(suite) -> [];
'ibm-not-wf-P24-ibm24n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P24-ibm24n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P24-ibm24n05'(suite) -> [];
'ibm-not-wf-P24-ibm24n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P24-ibm24n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P24-ibm24n04'(suite) -> [];
'ibm-not-wf-P24-ibm24n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P24-ibm24n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P24-ibm24n03'(suite) -> [];
'ibm-not-wf-P24-ibm24n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P24-ibm24n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P24-ibm24n02'(suite) -> [];
'ibm-not-wf-P24-ibm24n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P24-ibm24n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P24-ibm24n01'(suite) -> [];
'ibm-not-wf-P24-ibm24n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P24-ibm24n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P23-ibm23n06'(suite) -> [];
'ibm-not-wf-P23-ibm23n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P23-ibm23n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P23-ibm23n05'(suite) -> [];
'ibm-not-wf-P23-ibm23n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P23-ibm23n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P23-ibm23n04'(suite) -> [];
'ibm-not-wf-P23-ibm23n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P23-ibm23n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P23-ibm23n03'(suite) -> [];
'ibm-not-wf-P23-ibm23n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P23-ibm23n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P23-ibm23n02'(suite) -> [];
'ibm-not-wf-P23-ibm23n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P23-ibm23n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P23-ibm23n01'(suite) -> [];
'ibm-not-wf-P23-ibm23n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P23-ibm23n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P22-ibm22n03'(suite) -> [];
'ibm-not-wf-P22-ibm22n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P22-ibm22n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P22-ibm22n02'(suite) -> [];
'ibm-not-wf-P22-ibm22n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P22-ibm22n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P22-ibm22n01'(suite) -> [];
'ibm-not-wf-P22-ibm22n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P22-ibm22n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P21-ibm21n03'(suite) -> [];
'ibm-not-wf-P21-ibm21n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P21-ibm21n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P21-ibm21n02'(suite) -> [];
'ibm-not-wf-P21-ibm21n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P21-ibm21n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P21-ibm21n01'(suite) -> [];
'ibm-not-wf-P21-ibm21n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P21-ibm21n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P20-ibm20n01'(suite) -> [];
'ibm-not-wf-P20-ibm20n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P20-ibm20n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P19-ibm19n03'(suite) -> [];
'ibm-not-wf-P19-ibm19n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P19-ibm19n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P19-ibm19n02'(suite) -> [];
'ibm-not-wf-P19-ibm19n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P19-ibm19n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P19-ibm19n01'(suite) -> [];
'ibm-not-wf-P19-ibm19n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P19-ibm19n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P18-ibm18n02'(suite) -> [];
'ibm-not-wf-P18-ibm18n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P18-ibm18n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P18-ibm18n01'(suite) -> [];
'ibm-not-wf-P18-ibm18n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P18-ibm18n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P17-ibm17n04'(suite) -> [];
'ibm-not-wf-P17-ibm17n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P17-ibm17n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P17-ibm17n03'(suite) -> [];
'ibm-not-wf-P17-ibm17n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P17-ibm17n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P17-ibm17n02'(suite) -> [];
'ibm-not-wf-P17-ibm17n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P17-ibm17n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P17-ibm17n01'(suite) -> [];
'ibm-not-wf-P17-ibm17n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P17-ibm17n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P16-ibm16n04'(suite) -> [];
'ibm-not-wf-P16-ibm16n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P16-ibm16n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P16-ibm16n03'(suite) -> [];
'ibm-not-wf-P16-ibm16n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P16-ibm16n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P16-ibm16n02'(suite) -> [];
'ibm-not-wf-P16-ibm16n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P16-ibm16n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P16-ibm16n01'(suite) -> [];
'ibm-not-wf-P16-ibm16n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P16-ibm16n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P15-ibm15n04'(suite) -> [];
'ibm-not-wf-P15-ibm15n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P15-ibm15n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P15-ibm15n03'(suite) -> [];
'ibm-not-wf-P15-ibm15n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P15-ibm15n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P15-ibm15n02'(suite) -> [];
'ibm-not-wf-P15-ibm15n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P15-ibm15n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P15-ibm15n01'(suite) -> [];
'ibm-not-wf-P15-ibm15n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P15-ibm15n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P14-ibm14n03'(suite) -> [];
'ibm-not-wf-P14-ibm14n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P14-ibm14n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P14-ibm14n02'(suite) -> [];
'ibm-not-wf-P14-ibm14n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P14-ibm14n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P14-ibm14n01'(suite) -> [];
'ibm-not-wf-P14-ibm14n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P14-ibm14n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P13-ibm13n03'(suite) -> [];
'ibm-not-wf-P13-ibm13n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P13-ibm13n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P13-ibm13n02'(suite) -> [];
'ibm-not-wf-P13-ibm13n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P13-ibm13n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P13-ibm13n01'(suite) -> [];
'ibm-not-wf-P13-ibm13n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P13-ibm13n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P12-ibm12n03'(suite) -> [];
'ibm-not-wf-P12-ibm12n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P12-ibm12n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P12-ibm12n02'(suite) -> [];
'ibm-not-wf-P12-ibm12n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P12-ibm12n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P12-ibm12n01'(suite) -> [];
'ibm-not-wf-P12-ibm12n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P12-ibm12n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P11-ibm11n04'(suite) -> [];
'ibm-not-wf-P11-ibm11n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P11-ibm11n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P11-ibm11n03'(suite) -> [];
'ibm-not-wf-P11-ibm11n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P11-ibm11n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P11-ibm11n02'(suite) -> [];
'ibm-not-wf-P11-ibm11n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P11-ibm11n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P11-ibm11n01'(suite) -> [];
'ibm-not-wf-P11-ibm11n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P11-ibm11n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P10-ibm10n08'(suite) -> [];
'ibm-not-wf-P10-ibm10n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P10-ibm10n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P10-ibm10n07'(suite) -> [];
'ibm-not-wf-P10-ibm10n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P10-ibm10n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P10-ibm10n06'(suite) -> [];
'ibm-not-wf-P10-ibm10n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P10-ibm10n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P10-ibm10n05'(suite) -> [];
'ibm-not-wf-P10-ibm10n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P10-ibm10n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P10-ibm10n04'(suite) -> [];
'ibm-not-wf-P10-ibm10n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P10-ibm10n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P10-ibm10n03'(suite) -> [];
'ibm-not-wf-P10-ibm10n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P10-ibm10n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P10-ibm10n02'(suite) -> [];
'ibm-not-wf-P10-ibm10n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P10-ibm10n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P10-ibm10n01'(suite) -> [];
'ibm-not-wf-P10-ibm10n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P10-ibm10n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P09-ibm09n04'(suite) -> [];
'ibm-not-wf-P09-ibm09n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P09-ibm09n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P09-ibm09n03'(suite) -> [];
'ibm-not-wf-P09-ibm09n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P09-ibm09n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P09-ibm09n02'(suite) -> [];
'ibm-not-wf-P09-ibm09n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P09-ibm09n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P09-ibm09n01'(suite) -> [];
'ibm-not-wf-P09-ibm09n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P09-ibm09n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P05-ibm05n03'(suite) -> [];
'ibm-not-wf-P05-ibm05n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P05-ibm05n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P05-ibm05n02'(suite) -> [];
'ibm-not-wf-P05-ibm05n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P05-ibm05n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P05-ibm05n01'(suite) -> [];
'ibm-not-wf-P05-ibm05n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P05-ibm05n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n18'(suite) -> [];
'ibm-not-wf-P04-ibm04n18'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n18.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n17'(suite) -> [];
'ibm-not-wf-P04-ibm04n17'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n17.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n16'(suite) -> [];
'ibm-not-wf-P04-ibm04n16'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n16.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n15'(suite) -> [];
'ibm-not-wf-P04-ibm04n15'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n15.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n14'(suite) -> [];
'ibm-not-wf-P04-ibm04n14'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n14.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n13'(suite) -> [];
'ibm-not-wf-P04-ibm04n13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n12'(suite) -> [];
'ibm-not-wf-P04-ibm04n12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n11'(suite) -> [];
'ibm-not-wf-P04-ibm04n11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n10'(suite) -> [];
'ibm-not-wf-P04-ibm04n10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n09'(suite) -> [];
'ibm-not-wf-P04-ibm04n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n08'(suite) -> [];
'ibm-not-wf-P04-ibm04n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n07'(suite) -> [];
'ibm-not-wf-P04-ibm04n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n06'(suite) -> [];
'ibm-not-wf-P04-ibm04n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n05'(suite) -> [];
'ibm-not-wf-P04-ibm04n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n04'(suite) -> [];
'ibm-not-wf-P04-ibm04n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n03'(suite) -> [];
'ibm-not-wf-P04-ibm04n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n02'(suite) -> [];
'ibm-not-wf-P04-ibm04n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P04-ibm04n01'(suite) -> [];
'ibm-not-wf-P04-ibm04n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P04-ibm04n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P03-ibm03n01'(suite) -> [];
'ibm-not-wf-P03-ibm03n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P03-ibm03n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n33'(suite) -> [];
'ibm-not-wf-P02-ibm02n33'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n33.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n32'(suite) -> [];
'ibm-not-wf-P02-ibm02n32'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n32.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n31'(suite) -> [];
'ibm-not-wf-P02-ibm02n31'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n31.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n30'(suite) -> [];
'ibm-not-wf-P02-ibm02n30'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n30.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n29'(suite) -> [];
'ibm-not-wf-P02-ibm02n29'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n29.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n28'(suite) -> [];
'ibm-not-wf-P02-ibm02n28'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n28.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n27'(suite) -> [];
'ibm-not-wf-P02-ibm02n27'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n27.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n26'(suite) -> [];
'ibm-not-wf-P02-ibm02n26'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n26.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n25'(suite) -> [];
'ibm-not-wf-P02-ibm02n25'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n25.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n24'(suite) -> [];
'ibm-not-wf-P02-ibm02n24'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n24.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n23'(suite) -> [];
'ibm-not-wf-P02-ibm02n23'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n23.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n22'(suite) -> [];
'ibm-not-wf-P02-ibm02n22'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n22.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n21'(suite) -> [];
'ibm-not-wf-P02-ibm02n21'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n21.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n20'(suite) -> [];
'ibm-not-wf-P02-ibm02n20'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n20.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n19'(suite) -> [];
'ibm-not-wf-P02-ibm02n19'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n19.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n18'(suite) -> [];
'ibm-not-wf-P02-ibm02n18'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n18.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n17'(suite) -> [];
'ibm-not-wf-P02-ibm02n17'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n17.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n16'(suite) -> [];
'ibm-not-wf-P02-ibm02n16'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n16.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n15'(suite) -> [];
'ibm-not-wf-P02-ibm02n15'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n15.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n14'(suite) -> [];
'ibm-not-wf-P02-ibm02n14'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n14.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n13'(suite) -> [];
'ibm-not-wf-P02-ibm02n13'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n13.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n12'(suite) -> [];
'ibm-not-wf-P02-ibm02n12'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n12.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n11'(suite) -> [];
'ibm-not-wf-P02-ibm02n11'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n11.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n10'(suite) -> [];
'ibm-not-wf-P02-ibm02n10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n09'(suite) -> [];
'ibm-not-wf-P02-ibm02n09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n08'(suite) -> [];
'ibm-not-wf-P02-ibm02n08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n07'(suite) -> [];
'ibm-not-wf-P02-ibm02n07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n06'(suite) -> [];
'ibm-not-wf-P02-ibm02n06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n05'(suite) -> [];
'ibm-not-wf-P02-ibm02n05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n04'(suite) -> [];
'ibm-not-wf-P02-ibm02n04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n03'(suite) -> [];
'ibm-not-wf-P02-ibm02n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n02'(suite) -> [];
'ibm-not-wf-P02-ibm02n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P02-ibm02n01'(suite) -> [];
'ibm-not-wf-P02-ibm02n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P02-ibm02n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P01-ibm01n03'(suite) -> [];
'ibm-not-wf-P01-ibm01n03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P01-ibm01n03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P01-ibm01n02'(suite) -> [];
'ibm-not-wf-P01-ibm01n02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P01-ibm01n02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-not-wf-P01-ibm01n01'(suite) -> [];
'ibm-not-wf-P01-ibm01n01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-not-wf-P01-ibm01n01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

%%----------------------------------------------------------------------

'ibm-valid-P89-ibm89v01'(suite) -> [];
'ibm-valid-P89-ibm89v01'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P89-ibm89v01.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Fails to handle name containing characters > x#ff, since they are converted to atoms"]}.

'ibm-valid-P88-ibm88v01'(suite) -> [];
'ibm-valid-P88-ibm88v01'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P88-ibm88v01.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Fails to handle name containing characters > x#ff, since they are converted to atoms"]}.

'ibm-valid-P87-ibm87v01'(suite) -> [];
'ibm-valid-P87-ibm87v01'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P87-ibm87v01.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Fails to handle name containing characters > x#ff, since they are converted to atoms"]}.

'ibm-valid-P86-ibm86v01'(suite) -> [];
'ibm-valid-P86-ibm86v01'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P86-ibm86v01.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Fails to handle name containing characters > x#ff, since they are converted to atoms"]}.

'ibm-valid-P85-ibm85v01'(suite) -> [];
'ibm-valid-P85-ibm85v01'(_Config) ->
%   ?line file:set_cwd(?config(data_dir,Config)),
%   ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P85-ibm85v01.xml"]),[]),
%   ?line xmerl:export([A],xmerl_test).
    {skip,["Fails to handle name containing characters > x#ff, since they are converted to atoms"]}.

'ibm-valid-P82-ibm82v01'(suite) -> [];
'ibm-valid-P82-ibm82v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P82-ibm82v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P79-ibm79v01'(suite) -> [];
'ibm-valid-P79-ibm79v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P79-ibm79v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P78-ibm78v01'(suite) -> [];
'ibm-valid-P78-ibm78v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P78-ibm78v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P70-ibm70v01'(suite) -> [];
'ibm-valid-P70-ibm70v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P70-ibm70v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P69-ibm69v02'(suite) -> [];
'ibm-valid-P69-ibm69v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P69-ibm69v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P69-ibm69v01'(suite) -> [];
'ibm-valid-P69-ibm69v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P69-ibm69v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P68-ibm68v02'(suite) -> [];
'ibm-valid-P68-ibm68v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P68-ibm68v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P68-ibm68v01'(suite) -> [];
'ibm-valid-P68-ibm68v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P68-ibm68v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P67-ibm67v01'(suite) -> [];
'ibm-valid-P67-ibm67v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P67-ibm67v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P66-ibm66v01'(suite) -> [];
'ibm-valid-P66-ibm66v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P66-ibm66v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P65-ibm65v02'(suite) -> [];
'ibm-valid-P65-ibm65v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P65-ibm65v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P65-ibm65v01'(suite) -> [];
'ibm-valid-P65-ibm65v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P65-ibm65v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P64-ibm64v03'(suite) -> [];
'ibm-valid-P64-ibm64v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P64-ibm64v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P64-ibm64v02'(suite) -> [];
'ibm-valid-P64-ibm64v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P64-ibm64v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P64-ibm64v01'(suite) -> [];
'ibm-valid-P64-ibm64v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P64-ibm64v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P63-ibm63v05'(suite) -> [];
'ibm-valid-P63-ibm63v05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P63-ibm63v05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P63-ibm63v04'(suite) -> [];
'ibm-valid-P63-ibm63v04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P63-ibm63v04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P63-ibm63v03'(suite) -> [];
'ibm-valid-P63-ibm63v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P63-ibm63v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P63-ibm63v02'(suite) -> [];
'ibm-valid-P63-ibm63v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P63-ibm63v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P63-ibm63v01'(suite) -> [];
'ibm-valid-P63-ibm63v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P63-ibm63v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P62-ibm62v05'(suite) -> [];
'ibm-valid-P62-ibm62v05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P62-ibm62v05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P62-ibm62v04'(suite) -> [];
'ibm-valid-P62-ibm62v04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P62-ibm62v04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P62-ibm62v03'(suite) -> [];
'ibm-valid-P62-ibm62v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P62-ibm62v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P62-ibm62v02'(suite) -> [];
'ibm-valid-P62-ibm62v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P62-ibm62v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P62-ibm62v01'(suite) -> [];
'ibm-valid-P62-ibm62v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P62-ibm62v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P61-ibm61v02'(suite) -> [];
'ibm-valid-P61-ibm61v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P61-ibm61v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P61-ibm61v01'(suite) -> [];
'ibm-valid-P61-ibm61v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P61-ibm61v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P60-ibm60v04'(suite) -> [];
'ibm-valid-P60-ibm60v04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P60-ibm60v04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P60-ibm60v03'(suite) -> [];
'ibm-valid-P60-ibm60v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P60-ibm60v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P60-ibm60v02'(suite) -> [];
'ibm-valid-P60-ibm60v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P60-ibm60v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P60-ibm60v01'(suite) -> [];
'ibm-valid-P60-ibm60v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P60-ibm60v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P59-ibm59v02'(suite) -> [];
'ibm-valid-P59-ibm59v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P59-ibm59v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P59-ibm59v01'(suite) -> [];
'ibm-valid-P59-ibm59v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P59-ibm59v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P58-ibm58v02'(suite) -> [];
'ibm-valid-P58-ibm58v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P58-ibm58v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P58-ibm58v01'(suite) -> [];
'ibm-valid-P58-ibm58v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P58-ibm58v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P57-ibm57v01'(suite) -> [];
'ibm-valid-P57-ibm57v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P57-ibm57v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P56-ibm56v10'(suite) -> [];
'ibm-valid-P56-ibm56v10'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P56-ibm56v10.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P56-ibm56v09'(suite) -> [];
'ibm-valid-P56-ibm56v09'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P56-ibm56v09.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P56-ibm56v08'(suite) -> [];
'ibm-valid-P56-ibm56v08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P56-ibm56v08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P56-ibm56v07'(suite) -> [];
'ibm-valid-P56-ibm56v07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P56-ibm56v07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P56-ibm56v06'(suite) -> [];
'ibm-valid-P56-ibm56v06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P56-ibm56v06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P56-ibm56v05'(suite) -> [];
'ibm-valid-P56-ibm56v05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P56-ibm56v05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P56-ibm56v04'(suite) -> [];
'ibm-valid-P56-ibm56v04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P56-ibm56v04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P56-ibm56v03'(suite) -> [];
'ibm-valid-P56-ibm56v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P56-ibm56v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P56-ibm56v02'(suite) -> [];
'ibm-valid-P56-ibm56v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P56-ibm56v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P56-ibm56v01'(suite) -> [];
'ibm-valid-P56-ibm56v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P56-ibm56v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P55-ibm55v01'(suite) -> [];
'ibm-valid-P55-ibm55v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P55-ibm55v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P54-ibm54v03'(suite) -> [];
'ibm-valid-P54-ibm54v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P54-ibm54v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P54-ibm54v02'(suite) -> [];
'ibm-valid-P54-ibm54v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P54-ibm54v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P54-ibm54v01'(suite) -> [];
'ibm-valid-P54-ibm54v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P54-ibm54v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P52-ibm52v01'(suite) -> [];
'ibm-valid-P52-ibm52v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P52-ibm52v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P51-ibm51v02'(suite) -> [];
'ibm-valid-P51-ibm51v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P51-ibm51v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P51-ibm51v01'(suite) -> [];
'ibm-valid-P51-ibm51v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P51-ibm51v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P50-ibm50v01'(suite) -> [];
'ibm-valid-P50-ibm50v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P50-ibm50v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P49-ibm49v01'(suite) -> [];
'ibm-valid-P49-ibm49v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P49-ibm49v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P47-ibm47v01'(suite) -> [];
'ibm-valid-P47-ibm47v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P47-ibm47v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P45-ibm45v01'(suite) -> [];
'ibm-valid-P45-ibm45v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P45-ibm45v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P44-ibm44v01'(suite) -> [];
'ibm-valid-P44-ibm44v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P44-ibm44v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P43-ibm43v01'(suite) -> [];
'ibm-valid-P43-ibm43v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P43-ibm43v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P42-ibm42v01'(suite) -> [];
'ibm-valid-P42-ibm42v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P42-ibm42v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P41-ibm41v01'(suite) -> [];
'ibm-valid-P41-ibm41v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P41-ibm41v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P40-ibm40v01'(suite) -> [];
'ibm-valid-P40-ibm40v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P40-ibm40v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P39-ibm39v01'(suite) -> [];
'ibm-valid-P39-ibm39v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P39-ibm39v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P38-ibm38v01'(suite) -> [];
'ibm-valid-P38-ibm38v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P38-ibm38v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P37-ibm37v01'(suite) -> [];
'ibm-valid-P37-ibm37v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P37-ibm37v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P36-ibm36v01'(suite) -> [];
'ibm-valid-P36-ibm36v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P36-ibm36v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P35-ibm35v01'(suite) -> [];
'ibm-valid-P35-ibm35v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P35-ibm35v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P34-ibm34v01'(suite) -> [];
'ibm-valid-P34-ibm34v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P34-ibm34v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P33-ibm33v01'(suite) -> [];
'ibm-valid-P33-ibm33v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P33-ibm33v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P32-ibm32v04'(suite) -> [];
'ibm-valid-P32-ibm32v04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P32-ibm32v04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P32-ibm32v03'(suite) -> [];
'ibm-valid-P32-ibm32v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P32-ibm32v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P32-ibm32v02'(suite) -> [];
'ibm-valid-P32-ibm32v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P32-ibm32v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P32-ibm32v01'(suite) -> [];
'ibm-valid-P32-ibm32v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P32-ibm32v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P31-ibm31v01'(suite) -> [];
'ibm-valid-P31-ibm31v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P31-ibm31v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P30-ibm30v02'(suite) -> [];
'ibm-valid-P30-ibm30v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P30-ibm30v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P30-ibm30v01'(suite) -> [];
'ibm-valid-P30-ibm30v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P30-ibm30v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P29-ibm29v02'(suite) -> [];
'ibm-valid-P29-ibm29v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P29-ibm29v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P29-ibm29v01'(suite) -> [];
'ibm-valid-P29-ibm29v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P29-ibm29v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P28-ibm28v02'(suite) -> [];
'ibm-valid-P28-ibm28v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P28-ibm28v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P28-ibm28v01'(suite) -> [];
'ibm-valid-P28-ibm28v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P28-ibm28v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P27-ibm27v03'(suite) -> [];
'ibm-valid-P27-ibm27v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P27-ibm27v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P27-ibm27v02'(suite) -> [];
'ibm-valid-P27-ibm27v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P27-ibm27v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P27-ibm27v01'(suite) -> [];
'ibm-valid-P27-ibm27v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P27-ibm27v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P26-ibm26v01'(suite) -> [];
'ibm-valid-P26-ibm26v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P26-ibm26v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P25-ibm25v04'(suite) -> [];
'ibm-valid-P25-ibm25v04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P25-ibm25v04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P25-ibm25v03'(suite) -> [];
'ibm-valid-P25-ibm25v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P25-ibm25v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P25-ibm25v02'(suite) -> [];
'ibm-valid-P25-ibm25v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P25-ibm25v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P25-ibm25v01'(suite) -> [];
'ibm-valid-P25-ibm25v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P25-ibm25v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P24-ibm24v02'(suite) -> [];
'ibm-valid-P24-ibm24v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P24-ibm24v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P24-ibm24v01'(suite) -> [];
'ibm-valid-P24-ibm24v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P24-ibm24v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P23-ibm23v06'(suite) -> [];
'ibm-valid-P23-ibm23v06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P23-ibm23v06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P23-ibm23v05'(suite) -> [];
'ibm-valid-P23-ibm23v05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P23-ibm23v05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P23-ibm23v04'(suite) -> [];
'ibm-valid-P23-ibm23v04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P23-ibm23v04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P23-ibm23v03'(suite) -> [];
'ibm-valid-P23-ibm23v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P23-ibm23v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P23-ibm23v02'(suite) -> [];
'ibm-valid-P23-ibm23v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P23-ibm23v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P23-ibm23v01'(suite) -> [];
'ibm-valid-P23-ibm23v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P23-ibm23v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P22-ibm22v07'(suite) -> [];
'ibm-valid-P22-ibm22v07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P22-ibm22v07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P22-ibm22v06'(suite) -> [];
'ibm-valid-P22-ibm22v06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P22-ibm22v06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P22-ibm22v05'(suite) -> [];
'ibm-valid-P22-ibm22v05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P22-ibm22v05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P22-ibm22v04'(suite) -> [];
'ibm-valid-P22-ibm22v04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P22-ibm22v04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P22-ibm22v03'(suite) -> [];
'ibm-valid-P22-ibm22v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P22-ibm22v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P22-ibm22v02'(suite) -> [];
'ibm-valid-P22-ibm22v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P22-ibm22v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P22-ibm22v01'(suite) -> [];
'ibm-valid-P22-ibm22v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P22-ibm22v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P21-ibm21v01'(suite) -> [];
'ibm-valid-P21-ibm21v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P21-ibm21v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P20-ibm20v02'(suite) -> [];
'ibm-valid-P20-ibm20v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P20-ibm20v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P20-ibm20v01'(suite) -> [];
'ibm-valid-P20-ibm20v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P20-ibm20v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P19-ibm19v01'(suite) -> [];
'ibm-valid-P19-ibm19v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P19-ibm19v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P18-ibm18v01'(suite) -> [];
'ibm-valid-P18-ibm18v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P18-ibm18v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P17-ibm17v01'(suite) -> [];
'ibm-valid-P17-ibm17v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P17-ibm17v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P16-ibm16v03'(suite) -> [];
'ibm-valid-P16-ibm16v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P16-ibm16v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P16-ibm16v02'(suite) -> [];
'ibm-valid-P16-ibm16v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P16-ibm16v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P16-ibm16v01'(suite) -> [];
'ibm-valid-P16-ibm16v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P16-ibm16v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P15-ibm15v04'(suite) -> [];
'ibm-valid-P15-ibm15v04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P15-ibm15v04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P15-ibm15v03'(suite) -> [];
'ibm-valid-P15-ibm15v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P15-ibm15v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P15-ibm15v02'(suite) -> [];
'ibm-valid-P15-ibm15v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P15-ibm15v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P15-ibm15v01'(suite) -> [];
'ibm-valid-P15-ibm15v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P15-ibm15v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P14-ibm14v03'(suite) -> [];
'ibm-valid-P14-ibm14v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P14-ibm14v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P14-ibm14v02'(suite) -> [];
'ibm-valid-P14-ibm14v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P14-ibm14v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P14-ibm14v01'(suite) -> [];
'ibm-valid-P14-ibm14v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P14-ibm14v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P13-ibm13v01'(suite) -> [];
'ibm-valid-P13-ibm13v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P13-ibm13v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P12-ibm12v04'(suite) -> [];
'ibm-valid-P12-ibm12v04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P12-ibm12v04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P12-ibm12v03'(suite) -> [];
'ibm-valid-P12-ibm12v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P12-ibm12v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P12-ibm12v02'(suite) -> [];
'ibm-valid-P12-ibm12v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P12-ibm12v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P12-ibm12v01'(suite) -> [];
'ibm-valid-P12-ibm12v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P12-ibm12v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P11-ibm11v04'(suite) -> [];
'ibm-valid-P11-ibm11v04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P11-ibm11v04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P11-ibm11v03'(suite) -> [];
'ibm-valid-P11-ibm11v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P11-ibm11v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P11-ibm11v02'(suite) -> [];
'ibm-valid-P11-ibm11v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P11-ibm11v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P11-ibm11v01'(suite) -> [];
'ibm-valid-P11-ibm11v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P11-ibm11v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P10-ibm10v08'(suite) -> [];
'ibm-valid-P10-ibm10v08'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P10-ibm10v08.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P10-ibm10v07'(suite) -> [];
'ibm-valid-P10-ibm10v07'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P10-ibm10v07.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P10-ibm10v06'(suite) -> [];
'ibm-valid-P10-ibm10v06'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P10-ibm10v06.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P10-ibm10v05'(suite) -> [];
'ibm-valid-P10-ibm10v05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P10-ibm10v05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P10-ibm10v04'(suite) -> [];
'ibm-valid-P10-ibm10v04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P10-ibm10v04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P10-ibm10v03'(suite) -> [];
'ibm-valid-P10-ibm10v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P10-ibm10v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P10-ibm10v02'(suite) -> [];
'ibm-valid-P10-ibm10v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P10-ibm10v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P10-ibm10v01'(suite) -> [];
'ibm-valid-P10-ibm10v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P10-ibm10v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P09-ibm09v05'(suite) -> [];
'ibm-valid-P09-ibm09v05'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P09-ibm09v05.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P09-ibm09v04'(suite) -> [];
'ibm-valid-P09-ibm09v04'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P09-ibm09v04.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P09-ibm09v03'(suite) -> [];
'ibm-valid-P09-ibm09v03'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P09-ibm09v03.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P09-ibm09v02'(suite) -> [];
'ibm-valid-P09-ibm09v02'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P09-ibm09v02.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P09-ibm09v01'(suite) -> [];
'ibm-valid-P09-ibm09v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P09-ibm09v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P03-ibm03v01'(suite) -> [];
'ibm-valid-P03-ibm03v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P03-ibm03v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P02-ibm02v01'(suite) -> [];
'ibm-valid-P02-ibm02v01'(Config) ->
  ?line file:set_cwd(?config(data_dir,Config)),
  ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P02-ibm02v01.xml"]),[]),
  ?line xmerl:export([A],xmerl_test).

'ibm-valid-P01-ibm01v01'(suite) -> [];
'ibm-valid-P01-ibm01v01'(Config) ->
    ?line file:set_cwd(?config(data_dir,Config)),
    ?line {A,_} = xmerl_scan:file(filename:join([?config(data_dir,Config),ibm,"ibm-valid-P01-ibm01v01.xml"]),[]),
    ?line xmerl:export([A],xmerl_test).



%%======================================================================
%% Support Functions
%%======================================================================

%% Dir is a directory
rm_f_(Dir) ->
    ?line {ok,CWD} = file:get_cwd(),
    ?line {ok,FileList} = file:list_dir(Dir),
    ?line file:set_cwd(filename:join([CWD,Dir])),
    rm_files(FileList),
    ?line file:set_cwd(CWD),
    ? line ok = file:del_dir(Dir).

rm_files([])->
    ok;
rm_files([F|Fs]) ->
    case filelib:is_dir(F) of
	true ->
	    rm_f_(F);
	_ ->
	    ?line ok = file:delete(F)
    end,
    rm_files(Fs).

-include_lib("kernel/include/file.hrl").
change_mode(Files) ->
    change_mode3(Files).
change_mode2(Dir)->
    ?line {ok,CWD} = file:get_cwd(),
    ?line {ok,FileList} = file:list_dir(Dir),
    ?line file:set_cwd(filename:join([CWD,Dir])),
    change_mode3(FileList),
    ?line file:set_cwd(CWD).
change_mode3([]) ->
    ok;
change_mode3([F|Fs]) ->
    case filelib:is_dir(F) of
	true ->
	    chmod(F),
	    change_mode2(F);
	_ ->
	    chmod(F)
    end,
    change_mode3(Fs).
    
chmod(F) ->
    case file:read_file_info(F) of
	{ok,FileInfo} ->
	    Mode= FileInfo#file_info.mode,
	    file:write_file_info(F,FileInfo#file_info{mode=8#00777 bor Mode});
	_ ->
	    ok
    end.
    




%%add_xml_path(TestCase) ->
%testcase_dir(TestCase) ->
%    add_xml_path(lists:member(TestCase,ibm_test_cases(suite)),?ibm_dir,TestCase,
%		[{fun japanese_test_cases/1,?japanese_dir},
%		 {fun oasis_test_cases/1,?oasis_dir},
%		 {fun sun_test_cases/1,?sun_dir},
%		 {fun xmltest_test_cases/1,?xmltest_dir}]).
%add_xml_path(true,Dir,_,_) ->
%    io:format("directory in path:~p~n",[Dir]),
%%    ?line code:add_patha(Dir);
%    Dir;
%add_xml_path(_,_,TestCase,[{NextTCs,NextDir}|Rest]) ->
%    add_xml_path(lists:member(TestCase,NextTCs(suite)),NextDir,TestCase,Rest);
%add_xml_path(false,_,TC,[]) ->
%    exit({error,{xmltests,uncovered_test_case,TC}}).

