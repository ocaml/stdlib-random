let major = Scanf.sscanf Sys.ocaml_version "%d.%_d.%_d" (fun x -> x)
let l =
  if major = 4 then
    (Random.init 100; List.init 100 (fun _ -> Random.float 1.))
else
[0X1.492D3BA4AF068P-3; 0X1.290622DDAC928P-5; 0X1.023476C14055BP-2;
 0X1.E85969D988151P-2; 0X1.4249AE8035CA7P-2; 0X1.7133CF7886B32P-1;
 0X1.0C3EE84F9C6B6P-2; 0X1.9306141CF9842P-3; 0X1.A0564884D81C4P-1;
 0X1.EFAB9633CC552P-1; 0X1.CF6D48D3BB259P-2; 0X1.935613FE03EEP-2;
 0X1.B3313FFFEA58AP-1; 0X1.599B31E5FB352P-1; 0X1.B67E6ECF84852P-2;
 0X1.B73CB8E45347P-1; 0X1.4800867295635P-4; 0X1.1A976A3F22C1AP-1;
 0X1.889765148A297P-1; 0X1.089DEA6720225P-1; 0X1.5E07C0564A807P-1;
 0X1.542A1E5DE6783P-2; 0X1.846AA6CE7BF43P-1; 0X1.56D6123A12ECAP-1;
 0X1.722602A664452P-5; 0X1.3A9BBC2FC9FA7P-2; 0X1.9BFD692598EF5P-4;
 0X1.161A3DED21A25P-3; 0X1.EAFF21902BE47P-2; 0X1.89B5AB5B0AE5P-5;
 0X1.17518635C5972P-3; 0X1.9B646832ADA5BP-1; 0X1.EA5D31DB2893AP-1;
 0X1.1617C6122F2CEP-1; 0X1.97A0D7283ADB6P-1; 0X1.914E545B6AFB8P-2;
 0X1.5FC89C17F2F04P-2; 0X1.661BE2A670298P-5; 0X1.14E39866C214BP-3;
 0X1.284E78210408EP-2; 0X1.780C947F294E8P-1; 0X1.1D581FA7DBA44P-1;
 0X1.E6EBE43C3AFB9P-1; 0X1.9A27A937F30D2P-1; 0X1.34DBFCA479FE8P-1;
 0X1.FB8F33B3332C5P-3; 0X1.CDC809FCE6BC7P-3; 0X1.73140B6A40253P-3;
 0X1.22690014864F3P-3; 0X1.ADB3E5EF00B35P-1; 0X1.F21A968C702DFP-1;
 0X1.97EF1CD87DF4FP-1; 0X1.25FFE73638357P-2; 0X1.98E8C51ACEBE7P-1;
 0X1.E5368123C5C4AP-2; 0X1.CFD02E0366B73P-1; 0X1.6314A409B6309P-1;
 0X1.AC6159535F37AP-3; 0X1.505969A166023P-2; 0X1.E8A4895CD6305P-1;
 0X1.BEFDFF1C7CE4BP-1; 0X1.DD22A0CE53EE5P-1; 0X1.161C2302C4B69P-5;
 0X1.1B0BEEB256C2DP-3; 0X1.67B5ECAF87E8EP-3; 0X1.882CC80710569P-1;
 0X1.FFF2C3C6EA6FAP-4; 0X1.22B49BA160B2DP-2; 0X1.5531584058644P-2;
 0X1.877777FAD9039P-1; 0X1.ACE22566CC393P-2; 0X1.8C0BC655A7DF6P-1;
 0X1.A49901497F0AP-3; 0X1.FB2C0D9BA7F09P-1; 0X1.D98E656C6E7AEP-1;
 0X1.9E7880E45698CP-3; 0X1.552868E81E61P-1; 0X1.271E58162B5A2P-1;
 0X1.44E969D00DF16P-1; 0X1.BF853CA77AC86P-1; 0X1.DF15038ECED6FP-3;
 0X1.6018205908AC7P-2; 0X1.05C42434CA666P-1; 0X1.E262B5C612A7DP-3;
 0X1.EC345F0036B54P-1; 0X1.06476B4FCA3F4P-1; 0X1.3F07D55BF4EE4P-2;
 0X1.B729CD85D2C14P-1; 0X1.FBDED9A28244P-3; 0X1.26B1916F613ABP-1;
 0X1.6AD5B9FFABC0AP-1; 0X1.0016CD5AB9847P-1; 0X1.4EEC2AEEBC579P-1;
 0X1.D67EABD5E9642P-4; 0X1.590E1BC273E12P-6; 0X1.EFF3EEA286FC5P-2;
 0X1.D75671EC8977BP-1; 0X1.8417D21BC1C3BP-6; 0X1.CB2253FF70D4EP-1;
 0X1.4B3BE1C02523AP-2]


let () =
  assert (Random4.init 100; List.init 100 (fun _ -> Random4.float 1.) = l )