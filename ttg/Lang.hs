module Lang where

import PGF hiding (Tree)
import qualified PGF
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> PGF.Tree
  fg :: PGF.Tree -> a

newtype GString = GString String  deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int  deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double  deriving Show

instance Gf GFloat where
  gf (GFloat x) = mkDouble x
  fg t =
    case unDouble t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GA =
   LexA String
  deriving Show

data GA2 =
   LexA2 String
  deriving Show

data GAP =
   GAdAP GAdA GAP 
 | GAdjOrd GOrd 
 | GAdvAP GAP GAdv 
 | GCAdvAP GCAdv GAP GNP 
 | GComparA GA GNP 
 | GComplA2 GA2 GNP 
 | GConjAP GConj GListAP 
 | GMarkupAP GMark GAP 
 | GPositA GA 
 | GReflA2 GA2 
 | GSentAP GAP GSC 
 | GUseA2 GA2 
 | GUseComparA GA 
 | Gn_units_AP GCard GCN GA 
  deriving Show

data GAdA =
   GPositAdAAdj GA 
 | LexAdA String
  deriving Show

data GAdN =
   GAdnCAdv GCAdv 
 | Galmost_AdN 
 | Gat_least_AdN 
 | Gat_most_AdN 
  deriving Show

data GAdV =
   GConjAdV GConj GListAdV 
 | Galways_AdV 
  deriving Show

data GAdv =
   GAdAdv GAdA GAdv 
 | GComparAdvAdj GCAdv GA GNP 
 | GComparAdvAdjS GCAdv GA GS 
 | GConjAdv GConj GListAdv 
 | GInLanguage GLanguage 
 | GMarkupAdv GMark GAdv 
 | GPositAdvAdj GA 
 | GPrepNP GPrep GNP 
 | GSubjS GSubj GS 
 | GdayMonthAdv GMonthday GMonth 
 | GdayMonthYearAdv GMonthday GMonth GYear 
 | GmonthAdv GMonth 
 | GmonthYearAdv GMonth GYear 
 | GtimeunitAdv GCard GTimeunit 
 | GweekdayHabitualAdv GWeekday 
 | GweekdayLastAdv GWeekday 
 | GweekdayNextAdv GWeekday 
 | GweekdayPunctualAdv GWeekday 
 | GyearAdv GYear 
 | LexAdv String
  deriving Show

data GAnt =
   GAAnter 
 | GASimul 
  deriving Show

data GCAdv =
   Gas_CAdv 
 | Gless_CAdv 
 | Gmore_CAdv 
  deriving Show

data GCN =
   GAdjCN GAP GCN 
 | GAdvCN GCN GAdv 
 | GApposCN GCN GNP 
 | GComplN2 GN2 GNP 
 | GConjCN GConj GListCN 
 | GMarkupCN GMark GCN 
 | GPartNP GCN GNP 
 | GPossNP GCN GNP 
 | GRelCN GCN GRS 
 | GSentCN GCN GSC 
 | GUseN GN 
 | GUseN2 GN2 
 | Gbottle_of_CN GNP 
 | Gcup_of_CN GNP 
 | Gglass_of_CN GNP 
 | GlanguageCN GLanguage 
  deriving Show

data GCard =
   GAdNum GAdN GCard 
 | GNumDigits GDigits 
 | GNumNumeral GNumeral 
 | Gdigits2numeral GCard 
  deriving Show

data GCl =
   GCleftAdv GAdv GS 
 | GCleftNP GNP GRS 
 | GExistNP GNP 
 | GExistNPAdv GNP GAdv 
 | GGenericCl GVP 
 | GImpersCl GVP 
 | GPredSCVP GSC GVP 
 | GPredVP GNP GVP 
 | Gactive2passive GCl 
 | Ghave_name_Cl GNP GNP 
 | Gmarried_Cl GNP GNP 
 | Gweather_adjCl GAP 
  deriving Show

data GClSlash =
   GAdvSlash GClSlash GAdv 
 | GSlashPrep GCl GPrep 
 | GSlashVP GNP GVPSlash 
 | GSlashVS GNP GVS GSSlash 
  deriving Show

data GComp =
   GCompAP GAP 
 | GCompAdv GAdv 
 | GCompCN GCN 
 | GCompNP GNP 
  deriving Show

data GConj =
   Gand_Conj 
 | Gboth7and_DConj 
 | Geither7or_DConj 
 | Gif_then_Conj 
 | Gor_Conj 
  deriving Show

data GDAP =
   GAdjDAP GDAP GAP 
 | GDetDAP GDet 
  deriving Show

data GDefinition =
   GMkDefinition GString GString 
 | GMkDefinitionEx GString GString GString 
 | GNoDefinition GString 
  deriving Show

data GDet =
   GConjDet GConj GListDAP 
 | GDetQuant GQuant GNum 
 | GDetQuantOrd GQuant GNum GOrd 
 | Gevery_Det 
 | Gfew_Det 
 | Gmany_Det 
 | Gmuch_Det 
 | GsomePl_Det 
 | GsomeSg_Det 
  deriving Show

data GDig =
   GD_0 
 | GD_1 
 | GD_2 
 | GD_3 
 | GD_4 
 | GD_5 
 | GD_6 
 | GD_7 
 | GD_8 
 | GD_9 
 | Gnd GDigit 
  deriving Show

data GDigit =
   Gdn GDig 
 | Gn2 
 | Gn3 
 | Gn4 
 | Gn5 
 | Gn6 
 | Gn7 
 | Gn8 
 | Gn9 
  deriving Show

data GDigits =
   GIDig GDig 
 | GIIDig GDig GDigits 
 | Gdconcat GDigits GDigits 
 | Gnd10 GSub10 
 | Gnd100 GSub100 
 | Gnd1000 GSub1000 
 | Gnd1000000 GSub1000000 
 | Gnum2digits GNumeral 
  deriving Show

data GDocument = GMkDocument GDefinition GInflection GString 
  deriving Show

data GIAdv =
   GAdvIAdv GIAdv GAdv 
 | GConjIAdv GConj GListIAdv 
 | GPrepIP GPrep GIP 
 | Ghow8much_IAdv 
 | Ghow_IAdv 
 | Gwhen_IAdv 
 | Gwhere_IAdv 
 | Gwhy_IAdv 
  deriving Show

data GIComp =
   GCompIAdv GIAdv 
 | GCompIP GIP 
  deriving Show

data GIDet =
   GIdetQuant GIQuant GNum 
 | Ghow8many_IDet 
  deriving Show

data GIP =
   GAdvIP GIP GAdv 
 | GIdetCN GIDet GCN 
 | GIdetIP GIDet 
 | GwhatPl_IP 
 | GwhatSg_IP 
 | GwhoPl_IP 
 | GwhoSg_IP 
  deriving Show

data GIQuant = Gwhich_IQuant 
  deriving Show

data GImp = GImpVP GVP 
  deriving Show

data GInflection =
   GInflectionA GA 
 | GInflectionA2 GA2 
 | GInflectionAdv GAdv 
 | GInflectionN GN 
 | GInflectionN2 GN2 
 | GInflectionN3 GN3 
 | GInflectionPrep GPrep 
 | GInflectionV GV 
 | GInflectionV2 GV2 
 | GInflectionV2A GV2A 
 | GInflectionV2Q GV2Q 
 | GInflectionV2S GV2S 
 | GInflectionV2V GV2V 
 | GInflectionV3 GV3 
 | GInflectionVA GVA 
 | GInflectionVQ GVQ 
 | GInflectionVS GVS 
 | GInflectionVV GVV 
  deriving Show

data GInterj = Galas_Interj 
  deriving Show

data GLanguage =
   Gafrikaans_Language 
 | Gamharic_Language 
 | Garabic_Language 
 | Gbulgarian_Language 
 | Gcatalan_Language 
 | Gchinese_Language 
 | Gdanish_Language 
 | Gdutch_Language 
 | Genglish_Language 
 | Gestonian_Language 
 | Gfinnish_Language 
 | Gfrench_Language 
 | Ggerman_Language 
 | Ggreek_Language 
 | Ghebrew_Language 
 | Ghindi_Language 
 | Gitalian_Language 
 | Gjapanese_Language 
 | Glatin_Language 
 | Glatvian_Language 
 | Gmaltese_Language 
 | Gnepali_Language 
 | Gnorwegian_Language 
 | Gpersian_Language 
 | Gpolish_Language 
 | Gpunjabi_Language 
 | Gromanian_Language 
 | Grussian_Language 
 | Gsindhi_Language 
 | Gspanish_Language 
 | Gswahili_Language 
 | Gswedish_Language 
 | Gthai_Language 
 | Gturkish_Language 
 | Gurdu_Language 
  deriving Show

newtype GListAP = GListAP [GAP] deriving Show

newtype GListAdV = GListAdV [GAdV] deriving Show

newtype GListAdv = GListAdv [GAdv] deriving Show

newtype GListCN = GListCN [GCN] deriving Show

newtype GListDAP = GListDAP [GDAP] deriving Show

newtype GListIAdv = GListIAdv [GIAdv] deriving Show

newtype GListNP = GListNP [GNP] deriving Show

newtype GListRS = GListRS [GRS] deriving Show

newtype GListS = GListS [GS] deriving Show

data GMark =
   Ga_Mark GString 
 | Gb_Mark 
 | Gh1_Mark 
 | Gh2_Mark 
 | Gi_Mark 
 | Gli_Mark 
 | Gp_Mark 
 | Gtable_Mark 
 | Gtd_Mark 
 | Gtr_Mark 
 | Gul_Mark 
  deriving Show

data GMonth =
   Gapril_Month 
 | Gaugust_Month 
 | Gdecember_Month 
 | Gfebruary_Month 
 | Gjanuary_Month 
 | Gjuly_Month 
 | Gjune_Month 
 | Gmarch_Month 
 | Gmay_Month 
 | Gnovember_Month 
 | Goctober_Month 
 | Gseptember_Month 
  deriving Show

data GMonthday = GintMonthday GInt 
  deriving Show

data GN =
   GmonthN GMonth 
 | GweekdayN GWeekday 
 | LexN String
  deriving Show

data GN2 =
   GComplN3 GN3 GNP 
 | GUse2N3 GN3 
 | GUse3N3 GN3 
 | LexN2 String
  deriving Show

data GN3 = LexN3 String
  deriving Show

data GNP =
   GAdvNP GNP GAdv 
 | GConjNP GConj GListNP 
 | GCountNP GDet GNP 
 | GDetCN GDet GCN 
 | GDetNP GDet 
 | GExtAdvNP GNP GAdv 
 | GMarkupNP GMark GNP 
 | GMassNP GCN 
 | GPPartNP GNP GV2 
 | GPredetNP GPredet GNP 
 | GRelNP GNP GRS 
 | GSelfNP GNP 
 | GUsePN GPN 
 | GUsePron GPron 
 | Geverybody_NP 
 | Geverything_NP 
 | GlanguageNP GLanguage 
 | Gn_units_of_NP GCard GCN GNP 
 | Gnobody_NP 
 | Gnothing_NP 
 | Gsomebody_NP 
 | Gsomething_NP 
  deriving Show

data GNum =
   GNumCard GCard 
 | GNumPl 
 | GNumSg 
  deriving Show

data GNumeral =
   Gdigits2num GDigits 
 | Gnum GSub1000000 
  deriving Show

data GOrd =
   GOrdDigits GDigits 
 | GOrdNumeral GNumeral 
 | GOrdNumeralSuperl GNumeral GA 
 | GOrdSuperl GA 
 | Gleft_Ord 
 | Gright_Ord 
  deriving Show

data GPConj =
   GNoPConj 
 | GPConjConj GConj 
 | Gbut_PConj 
 | Gotherwise_PConj 
 | Gtherefore_PConj 
  deriving Show

data GPN =
   GmonthPN GMonth 
 | GweekdayPN GWeekday 
 | LexPN String
  deriving Show

data GPhr =
   GMarkupPhr GMark GPhr 
 | GPhrUtt GPConj GUtt GVoc 
  deriving Show

data GPol =
   GPNeg 
 | GPPos 
  deriving Show

data GPredet =
   Gall_Predet 
 | Gmost_Predet 
 | Gnot_Predet 
 | Gonly_Predet 
  deriving Show

data GPrep =
   Gabove_Prep 
 | Gafter_Prep 
 | Gbefore_Prep 
 | Gbehind_Prep 
 | Gbetween_Prep 
 | Gby8agent_Prep 
 | Gby8means_Prep 
 | Gduring_Prep 
 | Gexcept_Prep 
 | Gfor_Prep 
 | Gfrom_Prep 
 | Gin8front_Prep 
 | Gin_Prep 
 | Gon_Prep 
 | Gpart_Prep 
 | Gpossess_Prep 
 | Gthrough_Prep 
 | Gto_Prep 
 | Gunder_Prep 
 | Gwith_Prep 
 | Gwithout_Prep 
  deriving Show

data GPron =
   Ghe_Pron 
 | Gi_Pron 
 | Git_Pron 
 | Gshe_Pron 
 | Gthey_Pron 
 | Gwe_Pron 
 | GyouPl_Pron 
 | GyouPol_Pron 
 | GyouSg_Pron 
  deriving Show

data GQCl =
   GExistIP GIP 
 | GExistIPAdv GIP GAdv 
 | GQuestCl GCl 
 | GQuestIAdv GIAdv GCl 
 | GQuestIComp GIComp GNP 
 | GQuestQVP GIP GQVP 
 | GQuestSlash GIP GClSlash 
 | GQuestVP GIP GVP 
 | Ghow_far_QCl GNP 
 | Ghow_old_QCl GNP 
 | Gwhat_name_QCl GNP 
  deriving Show

data GQS = GUseQCl GTemp GPol GQCl 
  deriving Show

data GQVP =
   GAddAdvQVP GQVP GIAdv 
 | GAdvQVP GVP GIAdv 
 | GComplSlashIP GVPSlash GIP 
  deriving Show

data GQuant =
   GDefArt 
 | GIndefArt 
 | GPossPron GPron 
 | Gno_Quant 
 | Gthat_Quant 
 | Gthis_Quant 
  deriving Show

data GRCl =
   GRelCl GCl 
 | GRelSlash GRP GClSlash 
 | GRelVP GRP GVP 
  deriving Show

data GRP =
   GFunRP GPrep GNP GRP 
 | GIdRP 
  deriving Show

data GRS =
   GConjRS GConj GListRS 
 | GUseRCl GTemp GPol GRCl 
  deriving Show

data GS =
   GAdvS GAdv GS 
 | GConjS GConj GListS 
 | GExtAdvS GAdv GS 
 | GMarkupS GMark GS 
 | GRelS GS GRS 
 | GSSubjS GS GSubj GS 
 | GUseCl GTemp GPol GCl 
 | Gfew_X_short_of_Y GNP GCN GCN 
  deriving Show

data GSC =
   GEmbedQS GQS 
 | GEmbedS GS 
 | GEmbedVP GVP 
  deriving Show

data GSSlash = GUseSlash GTemp GPol GClSlash 
  deriving Show

data GSub10 =
   Gdn10 GDig 
 | Gpot0 GDigit 
 | Gpot01 
  deriving Show

data GSub100 =
   Gdn100 GDig GDig 
 | Gpot0as1 GSub10 
 | Gpot1 GDigit 
 | Gpot110 
 | Gpot111 
 | Gpot1plus GDigit GSub10 
 | Gpot1to19 GDigit 
  deriving Show

data GSub1000 =
   Gdn1000 GDig GDig GDig 
 | Gpot1as2 GSub100 
 | Gpot2 GSub10 
 | Gpot2plus GSub10 GSub100 
  deriving Show

data GSub1000000 =
   Gdn1000000a GDig GDig GDig GDig 
 | Gdn1000000b GDig GDig GDig GDig GDig 
 | Gdn1000000c GDig GDig GDig GDig GDig GDig 
 | Gpot2as3 GSub1000 
 | Gpot3 GSub1000 
 | Gpot3plus GSub1000 GSub1000 
  deriving Show

data GSubj =
   Galthough_Subj 
 | Gbecause_Subj 
 | Gif_Subj 
 | Gthat_Subj 
 | Gwhen_Subj 
  deriving Show

data GTag = GMkTag GInflection 
  deriving Show

data GTemp = GTTAnt GTense GAnt 
  deriving Show

data GTense =
   GTCond 
 | GTFut 
 | GTPast 
 | GTPres 
  deriving Show

data GText =
   GMarkupText GMark GText 
 | GTEmpty 
 | GTExclMark GPhr GText 
 | GTFullStop GPhr GText 
 | GTQuestMark GPhr GText 
  deriving Show

data GTimeunit =
   Gday_Timeunit 
 | Ghour_Timeunit 
 | Gminute_Timeunit 
 | Gmonth_Timeunit 
 | Gsecond_Timeunit 
 | Gweek_Timeunit 
 | Gyear_Timeunit 
  deriving Show

data GUtt =
   GImpP3 GNP GVP 
 | GImpPl1 GVP 
 | GMarkupUtt GMark GUtt 
 | GUttAP GAP 
 | GUttAdv GAdv 
 | GUttCN GCN 
 | GUttCard GCard 
 | GUttIAdv GIAdv 
 | GUttIP GIP 
 | GUttImpPl GPol GImp 
 | GUttImpPol GPol GImp 
 | GUttImpSg GPol GImp 
 | GUttInterj GInterj 
 | GUttNP GNP 
 | GUttQS GQS 
 | GUttS GS 
 | GUttVP GVP 
 | Glanguage_title_Utt 
 | Gno_Utt 
 | Gyes_Utt 
  deriving Show

data GV =
   LexV String
  deriving Show

data GV2 =
   LexV2 String
  deriving Show

data GV2A = Gpaint_V2A 
  deriving Show

data GV2Q = LexV2Q String
  deriving Show

data GV2S = LexV2S String
  deriving Show

data GV2V = LexV2V String
  deriving Show

data GV3 =
   LexV3 String
  deriving Show

data GVA = Gbecome_VA 
  deriving Show

data GVP =
   GAdVVP GAdV GVP 
 | GAdvVP GVP GAdv 
 | GComplSlash GVPSlash GNP 
 | GComplVA GVA GAP 
 | GComplVQ GVQ GQS 
 | GComplVS GVS GS 
 | GComplVV GVV GVP 
 | GExtAdvVP GVP GAdv 
 | GPassV2 GV2 
 | GProgrVP GVP 
 | GReflVP GVPSlash 
 | GSelfAdVVP GVP 
 | GSelfAdvVP GVP 
 | GUseComp GComp 
 | GUseCopula 
 | GUseV GV 
 | Ghas_age_VP GCard 
 | Ghungry_VP 
 | Gill_VP 
 | Gis_right_VP 
 | Gis_wrong_VP 
 | Gready_VP 
 | Gscared_VP 
 | Gthirsty_VP 
 | Gtired_VP 
  deriving Show

data GVPSlash =
   GAdVVPSlash GAdV GVPSlash 
 | GAdvVPSlash GVPSlash GAdv 
 | GSlash2V3 GV3 GNP 
 | GSlash3V3 GV3 GNP 
 | GSlashV2A GV2A GAP 
 | GSlashV2Q GV2Q GQS 
 | GSlashV2S GV2S GS 
 | GSlashV2V GV2V GVP 
 | GSlashV2VNP GV2V GNP GVPSlash 
 | GSlashV2a GV2 
 | GSlashVV GVV GVPSlash 
 | GVPSlashPrep GVP GPrep 
  deriving Show

data GVQ =
   LexVQ String
  deriving Show

data GVS =
   LexVS String
  deriving Show

data GVV =
   Gcan8know_VV 
 | Gcan_VV 
 | Gmust_VV 
 | Gwant_VV 
  deriving Show

data GVoc =
   GNoVoc 
 | GVocNP GNP 
 | Gplease_Voc 
  deriving Show

data GWeekday =
   Gfriday_Weekday 
 | Gmonday_Weekday 
 | Gsaturday_Weekday 
 | Gsunday_Weekday 
 | Gthursday_Weekday 
 | Gtuesday_Weekday 
 | Gwednesday_Weekday 
  deriving Show

data GYear = GintYear GInt 
  deriving Show


instance Gf GA where
  gf (LexA x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexA (showCId i)
      _ -> error ("no A " ++ show t)

instance Gf GA2 where
  gf (LexA2 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexA2 (showCId i)
      _ -> error ("no A2 " ++ show t)

instance Gf GAP where
  gf (GAdAP x1 x2) = mkApp (mkCId "AdAP") [gf x1, gf x2]
  gf (GAdjOrd x1) = mkApp (mkCId "AdjOrd") [gf x1]
  gf (GAdvAP x1 x2) = mkApp (mkCId "AdvAP") [gf x1, gf x2]
  gf (GCAdvAP x1 x2 x3) = mkApp (mkCId "CAdvAP") [gf x1, gf x2, gf x3]
  gf (GComparA x1 x2) = mkApp (mkCId "ComparA") [gf x1, gf x2]
  gf (GComplA2 x1 x2) = mkApp (mkCId "ComplA2") [gf x1, gf x2]
  gf (GConjAP x1 x2) = mkApp (mkCId "ConjAP") [gf x1, gf x2]
  gf (GMarkupAP x1 x2) = mkApp (mkCId "MarkupAP") [gf x1, gf x2]
  gf (GPositA x1) = mkApp (mkCId "PositA") [gf x1]
  gf (GReflA2 x1) = mkApp (mkCId "ReflA2") [gf x1]
  gf (GSentAP x1 x2) = mkApp (mkCId "SentAP") [gf x1, gf x2]
  gf (GUseA2 x1) = mkApp (mkCId "UseA2") [gf x1]
  gf (GUseComparA x1) = mkApp (mkCId "UseComparA") [gf x1]
  gf (Gn_units_AP x1 x2 x3) = mkApp (mkCId "n_units_AP") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdAP" -> GAdAP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "AdjOrd" -> GAdjOrd (fg x1)
      Just (i,[x1,x2]) | i == mkCId "AdvAP" -> GAdvAP (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "CAdvAP" -> GCAdvAP (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "ComparA" -> GComparA (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplA2" -> GComplA2 (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjAP" -> GConjAP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "MarkupAP" -> GMarkupAP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "PositA" -> GPositA (fg x1)
      Just (i,[x1]) | i == mkCId "ReflA2" -> GReflA2 (fg x1)
      Just (i,[x1,x2]) | i == mkCId "SentAP" -> GSentAP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UseA2" -> GUseA2 (fg x1)
      Just (i,[x1]) | i == mkCId "UseComparA" -> GUseComparA (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "n_units_AP" -> Gn_units_AP (fg x1) (fg x2) (fg x3)


      _ -> error ("no AP " ++ show t)

instance Gf GAdA where
  gf (GPositAdAAdj x1) = mkApp (mkCId "PositAdAAdj") [gf x1]
  gf (LexAdA x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "PositAdAAdj" -> GPositAdAAdj (fg x1)

      Just (i,[]) -> LexAdA (showCId i)
      _ -> error ("no AdA " ++ show t)

instance Gf GAdN where
  gf (GAdnCAdv x1) = mkApp (mkCId "AdnCAdv") [gf x1]
  gf Galmost_AdN = mkApp (mkCId "almost_AdN") []
  gf Gat_least_AdN = mkApp (mkCId "at_least_AdN") []
  gf Gat_most_AdN = mkApp (mkCId "at_most_AdN") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "AdnCAdv" -> GAdnCAdv (fg x1)
      Just (i,[]) | i == mkCId "almost_AdN" -> Galmost_AdN 
      Just (i,[]) | i == mkCId "at_least_AdN" -> Gat_least_AdN 
      Just (i,[]) | i == mkCId "at_most_AdN" -> Gat_most_AdN 


      _ -> error ("no AdN " ++ show t)

instance Gf GAdV where
  gf (GConjAdV x1 x2) = mkApp (mkCId "ConjAdV") [gf x1, gf x2]
  gf Galways_AdV = mkApp (mkCId "always_AdV") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjAdV" -> GConjAdV (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "always_AdV" -> Galways_AdV 


      _ -> error ("no AdV " ++ show t)

instance Gf GAdv where
  gf (GAdAdv x1 x2) = mkApp (mkCId "AdAdv") [gf x1, gf x2]
  gf (GComparAdvAdj x1 x2 x3) = mkApp (mkCId "ComparAdvAdj") [gf x1, gf x2, gf x3]
  gf (GComparAdvAdjS x1 x2 x3) = mkApp (mkCId "ComparAdvAdjS") [gf x1, gf x2, gf x3]
  gf (GConjAdv x1 x2) = mkApp (mkCId "ConjAdv") [gf x1, gf x2]
  gf (GInLanguage x1) = mkApp (mkCId "InLanguage") [gf x1]
  gf (GMarkupAdv x1 x2) = mkApp (mkCId "MarkupAdv") [gf x1, gf x2]
  gf (GPositAdvAdj x1) = mkApp (mkCId "PositAdvAdj") [gf x1]
  gf (GPrepNP x1 x2) = mkApp (mkCId "PrepNP") [gf x1, gf x2]
  gf (GSubjS x1 x2) = mkApp (mkCId "SubjS") [gf x1, gf x2]
  gf (GdayMonthAdv x1 x2) = mkApp (mkCId "dayMonthAdv") [gf x1, gf x2]
  gf (GdayMonthYearAdv x1 x2 x3) = mkApp (mkCId "dayMonthYearAdv") [gf x1, gf x2, gf x3]
  gf (GmonthAdv x1) = mkApp (mkCId "monthAdv") [gf x1]
  gf (GmonthYearAdv x1 x2) = mkApp (mkCId "monthYearAdv") [gf x1, gf x2]
  gf (GtimeunitAdv x1 x2) = mkApp (mkCId "timeunitAdv") [gf x1, gf x2]
  gf (GweekdayHabitualAdv x1) = mkApp (mkCId "weekdayHabitualAdv") [gf x1]
  gf (GweekdayLastAdv x1) = mkApp (mkCId "weekdayLastAdv") [gf x1]
  gf (GweekdayNextAdv x1) = mkApp (mkCId "weekdayNextAdv") [gf x1]
  gf (GweekdayPunctualAdv x1) = mkApp (mkCId "weekdayPunctualAdv") [gf x1]
  gf (GyearAdv x1) = mkApp (mkCId "yearAdv") [gf x1]
  gf (LexAdv x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdAdv" -> GAdAdv (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "ComparAdvAdj" -> GComparAdvAdj (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "ComparAdvAdjS" -> GComparAdvAdjS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "ConjAdv" -> GConjAdv (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "InLanguage" -> GInLanguage (fg x1)
      Just (i,[x1,x2]) | i == mkCId "MarkupAdv" -> GMarkupAdv (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "PositAdvAdj" -> GPositAdvAdj (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PrepNP" -> GPrepNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SubjS" -> GSubjS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "dayMonthAdv" -> GdayMonthAdv (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "dayMonthYearAdv" -> GdayMonthYearAdv (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "monthAdv" -> GmonthAdv (fg x1)
      Just (i,[x1,x2]) | i == mkCId "monthYearAdv" -> GmonthYearAdv (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "timeunitAdv" -> GtimeunitAdv (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "weekdayHabitualAdv" -> GweekdayHabitualAdv (fg x1)
      Just (i,[x1]) | i == mkCId "weekdayLastAdv" -> GweekdayLastAdv (fg x1)
      Just (i,[x1]) | i == mkCId "weekdayNextAdv" -> GweekdayNextAdv (fg x1)
      Just (i,[x1]) | i == mkCId "weekdayPunctualAdv" -> GweekdayPunctualAdv (fg x1)
      Just (i,[x1]) | i == mkCId "yearAdv" -> GyearAdv (fg x1)

      Just (i,[]) -> LexAdv (showCId i)
      _ -> error ("no Adv " ++ show t)

instance Gf GAnt where
  gf GAAnter = mkApp (mkCId "AAnter") []
  gf GASimul = mkApp (mkCId "ASimul") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "AAnter" -> GAAnter 
      Just (i,[]) | i == mkCId "ASimul" -> GASimul 


      _ -> error ("no Ant " ++ show t)

instance Gf GCAdv where
  gf Gas_CAdv = mkApp (mkCId "as_CAdv") []
  gf Gless_CAdv = mkApp (mkCId "less_CAdv") []
  gf Gmore_CAdv = mkApp (mkCId "more_CAdv") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "as_CAdv" -> Gas_CAdv 
      Just (i,[]) | i == mkCId "less_CAdv" -> Gless_CAdv 
      Just (i,[]) | i == mkCId "more_CAdv" -> Gmore_CAdv 


      _ -> error ("no CAdv " ++ show t)

instance Gf GCN where
  gf (GAdjCN x1 x2) = mkApp (mkCId "AdjCN") [gf x1, gf x2]
  gf (GAdvCN x1 x2) = mkApp (mkCId "AdvCN") [gf x1, gf x2]
  gf (GApposCN x1 x2) = mkApp (mkCId "ApposCN") [gf x1, gf x2]
  gf (GComplN2 x1 x2) = mkApp (mkCId "ComplN2") [gf x1, gf x2]
  gf (GConjCN x1 x2) = mkApp (mkCId "ConjCN") [gf x1, gf x2]
  gf (GMarkupCN x1 x2) = mkApp (mkCId "MarkupCN") [gf x1, gf x2]
  gf (GPartNP x1 x2) = mkApp (mkCId "PartNP") [gf x1, gf x2]
  gf (GPossNP x1 x2) = mkApp (mkCId "PossNP") [gf x1, gf x2]
  gf (GRelCN x1 x2) = mkApp (mkCId "RelCN") [gf x1, gf x2]
  gf (GSentCN x1 x2) = mkApp (mkCId "SentCN") [gf x1, gf x2]
  gf (GUseN x1) = mkApp (mkCId "UseN") [gf x1]
  gf (GUseN2 x1) = mkApp (mkCId "UseN2") [gf x1]
  gf (Gbottle_of_CN x1) = mkApp (mkCId "bottle_of_CN") [gf x1]
  gf (Gcup_of_CN x1) = mkApp (mkCId "cup_of_CN") [gf x1]
  gf (Gglass_of_CN x1) = mkApp (mkCId "glass_of_CN") [gf x1]
  gf (GlanguageCN x1) = mkApp (mkCId "languageCN") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdjCN" -> GAdjCN (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AdvCN" -> GAdvCN (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ApposCN" -> GApposCN (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplN2" -> GComplN2 (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjCN" -> GConjCN (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "MarkupCN" -> GMarkupCN (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PartNP" -> GPartNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PossNP" -> GPossNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RelCN" -> GRelCN (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SentCN" -> GSentCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UseN" -> GUseN (fg x1)
      Just (i,[x1]) | i == mkCId "UseN2" -> GUseN2 (fg x1)
      Just (i,[x1]) | i == mkCId "bottle_of_CN" -> Gbottle_of_CN (fg x1)
      Just (i,[x1]) | i == mkCId "cup_of_CN" -> Gcup_of_CN (fg x1)
      Just (i,[x1]) | i == mkCId "glass_of_CN" -> Gglass_of_CN (fg x1)
      Just (i,[x1]) | i == mkCId "languageCN" -> GlanguageCN (fg x1)


      _ -> error ("no CN " ++ show t)

instance Gf GCard where
  gf (GAdNum x1 x2) = mkApp (mkCId "AdNum") [gf x1, gf x2]
  gf (GNumDigits x1) = mkApp (mkCId "NumDigits") [gf x1]
  gf (GNumNumeral x1) = mkApp (mkCId "NumNumeral") [gf x1]
  gf (Gdigits2numeral x1) = mkApp (mkCId "digits2numeral") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdNum" -> GAdNum (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "NumDigits" -> GNumDigits (fg x1)
      Just (i,[x1]) | i == mkCId "NumNumeral" -> GNumNumeral (fg x1)
      Just (i,[x1]) | i == mkCId "digits2numeral" -> Gdigits2numeral (fg x1)


      _ -> error ("no Card " ++ show t)

instance Gf GCl where
  gf (GCleftAdv x1 x2) = mkApp (mkCId "CleftAdv") [gf x1, gf x2]
  gf (GCleftNP x1 x2) = mkApp (mkCId "CleftNP") [gf x1, gf x2]
  gf (GExistNP x1) = mkApp (mkCId "ExistNP") [gf x1]
  gf (GExistNPAdv x1 x2) = mkApp (mkCId "ExistNPAdv") [gf x1, gf x2]
  gf (GGenericCl x1) = mkApp (mkCId "GenericCl") [gf x1]
  gf (GImpersCl x1) = mkApp (mkCId "ImpersCl") [gf x1]
  gf (GPredSCVP x1 x2) = mkApp (mkCId "PredSCVP") [gf x1, gf x2]
  gf (GPredVP x1 x2) = mkApp (mkCId "PredVP") [gf x1, gf x2]
  gf (Gactive2passive x1) = mkApp (mkCId "active2passive") [gf x1]
  gf (Ghave_name_Cl x1 x2) = mkApp (mkCId "have_name_Cl") [gf x1, gf x2]
  gf (Gmarried_Cl x1 x2) = mkApp (mkCId "married_Cl") [gf x1, gf x2]
  gf (Gweather_adjCl x1) = mkApp (mkCId "weather_adjCl") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "CleftAdv" -> GCleftAdv (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "CleftNP" -> GCleftNP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ExistNP" -> GExistNP (fg x1)
      Just (i,[x1,x2]) | i == mkCId "ExistNPAdv" -> GExistNPAdv (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "GenericCl" -> GGenericCl (fg x1)
      Just (i,[x1]) | i == mkCId "ImpersCl" -> GImpersCl (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PredSCVP" -> GPredSCVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PredVP" -> GPredVP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "active2passive" -> Gactive2passive (fg x1)
      Just (i,[x1,x2]) | i == mkCId "have_name_Cl" -> Ghave_name_Cl (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "married_Cl" -> Gmarried_Cl (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "weather_adjCl" -> Gweather_adjCl (fg x1)


      _ -> error ("no Cl " ++ show t)

instance Gf GClSlash where
  gf (GAdvSlash x1 x2) = mkApp (mkCId "AdvSlash") [gf x1, gf x2]
  gf (GSlashPrep x1 x2) = mkApp (mkCId "SlashPrep") [gf x1, gf x2]
  gf (GSlashVP x1 x2) = mkApp (mkCId "SlashVP") [gf x1, gf x2]
  gf (GSlashVS x1 x2 x3) = mkApp (mkCId "SlashVS") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvSlash" -> GAdvSlash (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SlashPrep" -> GSlashPrep (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SlashVP" -> GSlashVP (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "SlashVS" -> GSlashVS (fg x1) (fg x2) (fg x3)


      _ -> error ("no ClSlash " ++ show t)

instance Gf GComp where
  gf (GCompAP x1) = mkApp (mkCId "CompAP") [gf x1]
  gf (GCompAdv x1) = mkApp (mkCId "CompAdv") [gf x1]
  gf (GCompCN x1) = mkApp (mkCId "CompCN") [gf x1]
  gf (GCompNP x1) = mkApp (mkCId "CompNP") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "CompAP" -> GCompAP (fg x1)
      Just (i,[x1]) | i == mkCId "CompAdv" -> GCompAdv (fg x1)
      Just (i,[x1]) | i == mkCId "CompCN" -> GCompCN (fg x1)
      Just (i,[x1]) | i == mkCId "CompNP" -> GCompNP (fg x1)


      _ -> error ("no Comp " ++ show t)

instance Gf GConj where
  gf Gand_Conj = mkApp (mkCId "and_Conj") []
  gf Gboth7and_DConj = mkApp (mkCId "both7and_DConj") []
  gf Geither7or_DConj = mkApp (mkCId "either7or_DConj") []
  gf Gif_then_Conj = mkApp (mkCId "if_then_Conj") []
  gf Gor_Conj = mkApp (mkCId "or_Conj") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "and_Conj" -> Gand_Conj 
      Just (i,[]) | i == mkCId "both7and_DConj" -> Gboth7and_DConj 
      Just (i,[]) | i == mkCId "either7or_DConj" -> Geither7or_DConj 
      Just (i,[]) | i == mkCId "if_then_Conj" -> Gif_then_Conj 
      Just (i,[]) | i == mkCId "or_Conj" -> Gor_Conj 


      _ -> error ("no Conj " ++ show t)

instance Gf GDAP where
  gf (GAdjDAP x1 x2) = mkApp (mkCId "AdjDAP") [gf x1, gf x2]
  gf (GDetDAP x1) = mkApp (mkCId "DetDAP") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdjDAP" -> GAdjDAP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "DetDAP" -> GDetDAP (fg x1)


      _ -> error ("no DAP " ++ show t)

instance Gf GDefinition where
  gf (GMkDefinition x1 x2) = mkApp (mkCId "MkDefinition") [gf x1, gf x2]
  gf (GMkDefinitionEx x1 x2 x3) = mkApp (mkCId "MkDefinitionEx") [gf x1, gf x2, gf x3]
  gf (GNoDefinition x1) = mkApp (mkCId "NoDefinition") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "MkDefinition" -> GMkDefinition (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "MkDefinitionEx" -> GMkDefinitionEx (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "NoDefinition" -> GNoDefinition (fg x1)


      _ -> error ("no Definition " ++ show t)

instance Gf GDet where
  gf (GConjDet x1 x2) = mkApp (mkCId "ConjDet") [gf x1, gf x2]
  gf (GDetQuant x1 x2) = mkApp (mkCId "DetQuant") [gf x1, gf x2]
  gf (GDetQuantOrd x1 x2 x3) = mkApp (mkCId "DetQuantOrd") [gf x1, gf x2, gf x3]
  gf Gevery_Det = mkApp (mkCId "every_Det") []
  gf Gfew_Det = mkApp (mkCId "few_Det") []
  gf Gmany_Det = mkApp (mkCId "many_Det") []
  gf Gmuch_Det = mkApp (mkCId "much_Det") []
  gf GsomePl_Det = mkApp (mkCId "somePl_Det") []
  gf GsomeSg_Det = mkApp (mkCId "someSg_Det") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjDet" -> GConjDet (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "DetQuant" -> GDetQuant (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "DetQuantOrd" -> GDetQuantOrd (fg x1) (fg x2) (fg x3)
      Just (i,[]) | i == mkCId "every_Det" -> Gevery_Det 
      Just (i,[]) | i == mkCId "few_Det" -> Gfew_Det 
      Just (i,[]) | i == mkCId "many_Det" -> Gmany_Det 
      Just (i,[]) | i == mkCId "much_Det" -> Gmuch_Det 
      Just (i,[]) | i == mkCId "somePl_Det" -> GsomePl_Det 
      Just (i,[]) | i == mkCId "someSg_Det" -> GsomeSg_Det 


      _ -> error ("no Det " ++ show t)

instance Gf GDig where
  gf GD_0 = mkApp (mkCId "D_0") []
  gf GD_1 = mkApp (mkCId "D_1") []
  gf GD_2 = mkApp (mkCId "D_2") []
  gf GD_3 = mkApp (mkCId "D_3") []
  gf GD_4 = mkApp (mkCId "D_4") []
  gf GD_5 = mkApp (mkCId "D_5") []
  gf GD_6 = mkApp (mkCId "D_6") []
  gf GD_7 = mkApp (mkCId "D_7") []
  gf GD_8 = mkApp (mkCId "D_8") []
  gf GD_9 = mkApp (mkCId "D_9") []
  gf (Gnd x1) = mkApp (mkCId "nd") [gf x1]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "D_0" -> GD_0 
      Just (i,[]) | i == mkCId "D_1" -> GD_1 
      Just (i,[]) | i == mkCId "D_2" -> GD_2 
      Just (i,[]) | i == mkCId "D_3" -> GD_3 
      Just (i,[]) | i == mkCId "D_4" -> GD_4 
      Just (i,[]) | i == mkCId "D_5" -> GD_5 
      Just (i,[]) | i == mkCId "D_6" -> GD_6 
      Just (i,[]) | i == mkCId "D_7" -> GD_7 
      Just (i,[]) | i == mkCId "D_8" -> GD_8 
      Just (i,[]) | i == mkCId "D_9" -> GD_9 
      Just (i,[x1]) | i == mkCId "nd" -> Gnd (fg x1)


      _ -> error ("no Dig " ++ show t)

instance Gf GDigit where
  gf (Gdn x1) = mkApp (mkCId "dn") [gf x1]
  gf Gn2 = mkApp (mkCId "n2") []
  gf Gn3 = mkApp (mkCId "n3") []
  gf Gn4 = mkApp (mkCId "n4") []
  gf Gn5 = mkApp (mkCId "n5") []
  gf Gn6 = mkApp (mkCId "n6") []
  gf Gn7 = mkApp (mkCId "n7") []
  gf Gn8 = mkApp (mkCId "n8") []
  gf Gn9 = mkApp (mkCId "n9") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "dn" -> Gdn (fg x1)
      Just (i,[]) | i == mkCId "n2" -> Gn2 
      Just (i,[]) | i == mkCId "n3" -> Gn3 
      Just (i,[]) | i == mkCId "n4" -> Gn4 
      Just (i,[]) | i == mkCId "n5" -> Gn5 
      Just (i,[]) | i == mkCId "n6" -> Gn6 
      Just (i,[]) | i == mkCId "n7" -> Gn7 
      Just (i,[]) | i == mkCId "n8" -> Gn8 
      Just (i,[]) | i == mkCId "n9" -> Gn9 


      _ -> error ("no Digit " ++ show t)

instance Gf GDigits where
  gf (GIDig x1) = mkApp (mkCId "IDig") [gf x1]
  gf (GIIDig x1 x2) = mkApp (mkCId "IIDig") [gf x1, gf x2]
  gf (Gdconcat x1 x2) = mkApp (mkCId "dconcat") [gf x1, gf x2]
  gf (Gnd10 x1) = mkApp (mkCId "nd10") [gf x1]
  gf (Gnd100 x1) = mkApp (mkCId "nd100") [gf x1]
  gf (Gnd1000 x1) = mkApp (mkCId "nd1000") [gf x1]
  gf (Gnd1000000 x1) = mkApp (mkCId "nd1000000") [gf x1]
  gf (Gnum2digits x1) = mkApp (mkCId "num2digits") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "IDig" -> GIDig (fg x1)
      Just (i,[x1,x2]) | i == mkCId "IIDig" -> GIIDig (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "dconcat" -> Gdconcat (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "nd10" -> Gnd10 (fg x1)
      Just (i,[x1]) | i == mkCId "nd100" -> Gnd100 (fg x1)
      Just (i,[x1]) | i == mkCId "nd1000" -> Gnd1000 (fg x1)
      Just (i,[x1]) | i == mkCId "nd1000000" -> Gnd1000000 (fg x1)
      Just (i,[x1]) | i == mkCId "num2digits" -> Gnum2digits (fg x1)


      _ -> error ("no Digits " ++ show t)

instance Gf GDocument where
  gf (GMkDocument x1 x2 x3) = mkApp (mkCId "MkDocument") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "MkDocument" -> GMkDocument (fg x1) (fg x2) (fg x3)


      _ -> error ("no Document " ++ show t)

instance Gf GIAdv where
  gf (GAdvIAdv x1 x2) = mkApp (mkCId "AdvIAdv") [gf x1, gf x2]
  gf (GConjIAdv x1 x2) = mkApp (mkCId "ConjIAdv") [gf x1, gf x2]
  gf (GPrepIP x1 x2) = mkApp (mkCId "PrepIP") [gf x1, gf x2]
  gf Ghow8much_IAdv = mkApp (mkCId "how8much_IAdv") []
  gf Ghow_IAdv = mkApp (mkCId "how_IAdv") []
  gf Gwhen_IAdv = mkApp (mkCId "when_IAdv") []
  gf Gwhere_IAdv = mkApp (mkCId "where_IAdv") []
  gf Gwhy_IAdv = mkApp (mkCId "why_IAdv") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvIAdv" -> GAdvIAdv (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjIAdv" -> GConjIAdv (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PrepIP" -> GPrepIP (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "how8much_IAdv" -> Ghow8much_IAdv 
      Just (i,[]) | i == mkCId "how_IAdv" -> Ghow_IAdv 
      Just (i,[]) | i == mkCId "when_IAdv" -> Gwhen_IAdv 
      Just (i,[]) | i == mkCId "where_IAdv" -> Gwhere_IAdv 
      Just (i,[]) | i == mkCId "why_IAdv" -> Gwhy_IAdv 


      _ -> error ("no IAdv " ++ show t)

instance Gf GIComp where
  gf (GCompIAdv x1) = mkApp (mkCId "CompIAdv") [gf x1]
  gf (GCompIP x1) = mkApp (mkCId "CompIP") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "CompIAdv" -> GCompIAdv (fg x1)
      Just (i,[x1]) | i == mkCId "CompIP" -> GCompIP (fg x1)


      _ -> error ("no IComp " ++ show t)

instance Gf GIDet where
  gf (GIdetQuant x1 x2) = mkApp (mkCId "IdetQuant") [gf x1, gf x2]
  gf Ghow8many_IDet = mkApp (mkCId "how8many_IDet") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "IdetQuant" -> GIdetQuant (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "how8many_IDet" -> Ghow8many_IDet 


      _ -> error ("no IDet " ++ show t)

instance Gf GIP where
  gf (GAdvIP x1 x2) = mkApp (mkCId "AdvIP") [gf x1, gf x2]
  gf (GIdetCN x1 x2) = mkApp (mkCId "IdetCN") [gf x1, gf x2]
  gf (GIdetIP x1) = mkApp (mkCId "IdetIP") [gf x1]
  gf GwhatPl_IP = mkApp (mkCId "whatPl_IP") []
  gf GwhatSg_IP = mkApp (mkCId "whatSg_IP") []
  gf GwhoPl_IP = mkApp (mkCId "whoPl_IP") []
  gf GwhoSg_IP = mkApp (mkCId "whoSg_IP") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvIP" -> GAdvIP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "IdetCN" -> GIdetCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "IdetIP" -> GIdetIP (fg x1)
      Just (i,[]) | i == mkCId "whatPl_IP" -> GwhatPl_IP 
      Just (i,[]) | i == mkCId "whatSg_IP" -> GwhatSg_IP 
      Just (i,[]) | i == mkCId "whoPl_IP" -> GwhoPl_IP 
      Just (i,[]) | i == mkCId "whoSg_IP" -> GwhoSg_IP 


      _ -> error ("no IP " ++ show t)

instance Gf GIQuant where
  gf Gwhich_IQuant = mkApp (mkCId "which_IQuant") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "which_IQuant" -> Gwhich_IQuant 


      _ -> error ("no IQuant " ++ show t)

instance Gf GImp where
  gf (GImpVP x1) = mkApp (mkCId "ImpVP") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ImpVP" -> GImpVP (fg x1)


      _ -> error ("no Imp " ++ show t)

instance Gf GInflection where
  gf (GInflectionA x1) = mkApp (mkCId "InflectionA") [gf x1]
  gf (GInflectionA2 x1) = mkApp (mkCId "InflectionA2") [gf x1]
  gf (GInflectionAdv x1) = mkApp (mkCId "InflectionAdv") [gf x1]
  gf (GInflectionN x1) = mkApp (mkCId "InflectionN") [gf x1]
  gf (GInflectionN2 x1) = mkApp (mkCId "InflectionN2") [gf x1]
  gf (GInflectionN3 x1) = mkApp (mkCId "InflectionN3") [gf x1]
  gf (GInflectionPrep x1) = mkApp (mkCId "InflectionPrep") [gf x1]
  gf (GInflectionV x1) = mkApp (mkCId "InflectionV") [gf x1]
  gf (GInflectionV2 x1) = mkApp (mkCId "InflectionV2") [gf x1]
  gf (GInflectionV2A x1) = mkApp (mkCId "InflectionV2A") [gf x1]
  gf (GInflectionV2Q x1) = mkApp (mkCId "InflectionV2Q") [gf x1]
  gf (GInflectionV2S x1) = mkApp (mkCId "InflectionV2S") [gf x1]
  gf (GInflectionV2V x1) = mkApp (mkCId "InflectionV2V") [gf x1]
  gf (GInflectionV3 x1) = mkApp (mkCId "InflectionV3") [gf x1]
  gf (GInflectionVA x1) = mkApp (mkCId "InflectionVA") [gf x1]
  gf (GInflectionVQ x1) = mkApp (mkCId "InflectionVQ") [gf x1]
  gf (GInflectionVS x1) = mkApp (mkCId "InflectionVS") [gf x1]
  gf (GInflectionVV x1) = mkApp (mkCId "InflectionVV") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "InflectionA" -> GInflectionA (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionA2" -> GInflectionA2 (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionAdv" -> GInflectionAdv (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionN" -> GInflectionN (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionN2" -> GInflectionN2 (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionN3" -> GInflectionN3 (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionPrep" -> GInflectionPrep (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionV" -> GInflectionV (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionV2" -> GInflectionV2 (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionV2A" -> GInflectionV2A (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionV2Q" -> GInflectionV2Q (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionV2S" -> GInflectionV2S (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionV2V" -> GInflectionV2V (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionV3" -> GInflectionV3 (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionVA" -> GInflectionVA (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionVQ" -> GInflectionVQ (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionVS" -> GInflectionVS (fg x1)
      Just (i,[x1]) | i == mkCId "InflectionVV" -> GInflectionVV (fg x1)


      _ -> error ("no Inflection " ++ show t)

instance Gf GInterj where
  gf Galas_Interj = mkApp (mkCId "alas_Interj") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "alas_Interj" -> Galas_Interj 


      _ -> error ("no Interj " ++ show t)

instance Gf GLanguage where
  gf Gafrikaans_Language = mkApp (mkCId "afrikaans_Language") []
  gf Gamharic_Language = mkApp (mkCId "amharic_Language") []
  gf Garabic_Language = mkApp (mkCId "arabic_Language") []
  gf Gbulgarian_Language = mkApp (mkCId "bulgarian_Language") []
  gf Gcatalan_Language = mkApp (mkCId "catalan_Language") []
  gf Gchinese_Language = mkApp (mkCId "chinese_Language") []
  gf Gdanish_Language = mkApp (mkCId "danish_Language") []
  gf Gdutch_Language = mkApp (mkCId "dutch_Language") []
  gf Genglish_Language = mkApp (mkCId "english_Language") []
  gf Gestonian_Language = mkApp (mkCId "estonian_Language") []
  gf Gfinnish_Language = mkApp (mkCId "finnish_Language") []
  gf Gfrench_Language = mkApp (mkCId "french_Language") []
  gf Ggerman_Language = mkApp (mkCId "german_Language") []
  gf Ggreek_Language = mkApp (mkCId "greek_Language") []
  gf Ghebrew_Language = mkApp (mkCId "hebrew_Language") []
  gf Ghindi_Language = mkApp (mkCId "hindi_Language") []
  gf Gitalian_Language = mkApp (mkCId "italian_Language") []
  gf Gjapanese_Language = mkApp (mkCId "japanese_Language") []
  gf Glatin_Language = mkApp (mkCId "latin_Language") []
  gf Glatvian_Language = mkApp (mkCId "latvian_Language") []
  gf Gmaltese_Language = mkApp (mkCId "maltese_Language") []
  gf Gnepali_Language = mkApp (mkCId "nepali_Language") []
  gf Gnorwegian_Language = mkApp (mkCId "norwegian_Language") []
  gf Gpersian_Language = mkApp (mkCId "persian_Language") []
  gf Gpolish_Language = mkApp (mkCId "polish_Language") []
  gf Gpunjabi_Language = mkApp (mkCId "punjabi_Language") []
  gf Gromanian_Language = mkApp (mkCId "romanian_Language") []
  gf Grussian_Language = mkApp (mkCId "russian_Language") []
  gf Gsindhi_Language = mkApp (mkCId "sindhi_Language") []
  gf Gspanish_Language = mkApp (mkCId "spanish_Language") []
  gf Gswahili_Language = mkApp (mkCId "swahili_Language") []
  gf Gswedish_Language = mkApp (mkCId "swedish_Language") []
  gf Gthai_Language = mkApp (mkCId "thai_Language") []
  gf Gturkish_Language = mkApp (mkCId "turkish_Language") []
  gf Gurdu_Language = mkApp (mkCId "urdu_Language") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "afrikaans_Language" -> Gafrikaans_Language 
      Just (i,[]) | i == mkCId "amharic_Language" -> Gamharic_Language 
      Just (i,[]) | i == mkCId "arabic_Language" -> Garabic_Language 
      Just (i,[]) | i == mkCId "bulgarian_Language" -> Gbulgarian_Language 
      Just (i,[]) | i == mkCId "catalan_Language" -> Gcatalan_Language 
      Just (i,[]) | i == mkCId "chinese_Language" -> Gchinese_Language 
      Just (i,[]) | i == mkCId "danish_Language" -> Gdanish_Language 
      Just (i,[]) | i == mkCId "dutch_Language" -> Gdutch_Language 
      Just (i,[]) | i == mkCId "english_Language" -> Genglish_Language 
      Just (i,[]) | i == mkCId "estonian_Language" -> Gestonian_Language 
      Just (i,[]) | i == mkCId "finnish_Language" -> Gfinnish_Language 
      Just (i,[]) | i == mkCId "french_Language" -> Gfrench_Language 
      Just (i,[]) | i == mkCId "german_Language" -> Ggerman_Language 
      Just (i,[]) | i == mkCId "greek_Language" -> Ggreek_Language 
      Just (i,[]) | i == mkCId "hebrew_Language" -> Ghebrew_Language 
      Just (i,[]) | i == mkCId "hindi_Language" -> Ghindi_Language 
      Just (i,[]) | i == mkCId "italian_Language" -> Gitalian_Language 
      Just (i,[]) | i == mkCId "japanese_Language" -> Gjapanese_Language 
      Just (i,[]) | i == mkCId "latin_Language" -> Glatin_Language 
      Just (i,[]) | i == mkCId "latvian_Language" -> Glatvian_Language 
      Just (i,[]) | i == mkCId "maltese_Language" -> Gmaltese_Language 
      Just (i,[]) | i == mkCId "nepali_Language" -> Gnepali_Language 
      Just (i,[]) | i == mkCId "norwegian_Language" -> Gnorwegian_Language 
      Just (i,[]) | i == mkCId "persian_Language" -> Gpersian_Language 
      Just (i,[]) | i == mkCId "polish_Language" -> Gpolish_Language 
      Just (i,[]) | i == mkCId "punjabi_Language" -> Gpunjabi_Language 
      Just (i,[]) | i == mkCId "romanian_Language" -> Gromanian_Language 
      Just (i,[]) | i == mkCId "russian_Language" -> Grussian_Language 
      Just (i,[]) | i == mkCId "sindhi_Language" -> Gsindhi_Language 
      Just (i,[]) | i == mkCId "spanish_Language" -> Gspanish_Language 
      Just (i,[]) | i == mkCId "swahili_Language" -> Gswahili_Language 
      Just (i,[]) | i == mkCId "swedish_Language" -> Gswedish_Language 
      Just (i,[]) | i == mkCId "thai_Language" -> Gthai_Language 
      Just (i,[]) | i == mkCId "turkish_Language" -> Gturkish_Language 
      Just (i,[]) | i == mkCId "urdu_Language" -> Gurdu_Language 


      _ -> error ("no Language " ++ show t)

instance Gf GListAP where
  gf (GListAP [x1,x2]) = mkApp (mkCId "BaseAP") [gf x1, gf x2]
  gf (GListAP (x:xs)) = mkApp (mkCId "ConsAP") [gf x, gf (GListAP xs)]
  fg t =
    GListAP (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAP" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAP" -> fg x1 : fgs x2


      _ -> error ("no ListAP " ++ show t)

instance Gf GListAdV where
  gf (GListAdV [x1,x2]) = mkApp (mkCId "BaseAdV") [gf x1, gf x2]
  gf (GListAdV (x:xs)) = mkApp (mkCId "ConsAdV") [gf x, gf (GListAdV xs)]
  fg t =
    GListAdV (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAdV" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAdV" -> fg x1 : fgs x2


      _ -> error ("no ListAdV " ++ show t)

instance Gf GListAdv where
  gf (GListAdv [x1,x2]) = mkApp (mkCId "BaseAdv") [gf x1, gf x2]
  gf (GListAdv (x:xs)) = mkApp (mkCId "ConsAdv") [gf x, gf (GListAdv xs)]
  fg t =
    GListAdv (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseAdv" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsAdv" -> fg x1 : fgs x2


      _ -> error ("no ListAdv " ++ show t)

instance Gf GListCN where
  gf (GListCN [x1,x2]) = mkApp (mkCId "BaseCN") [gf x1, gf x2]
  gf (GListCN (x:xs)) = mkApp (mkCId "ConsCN") [gf x, gf (GListCN xs)]
  fg t =
    GListCN (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseCN" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsCN" -> fg x1 : fgs x2


      _ -> error ("no ListCN " ++ show t)

instance Gf GListDAP where
  gf (GListDAP [x1,x2]) = mkApp (mkCId "BaseDAP") [gf x1, gf x2]
  gf (GListDAP (x:xs)) = mkApp (mkCId "ConsDAP") [gf x, gf (GListDAP xs)]
  fg t =
    GListDAP (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseDAP" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsDAP" -> fg x1 : fgs x2


      _ -> error ("no ListDAP " ++ show t)

instance Gf GListIAdv where
  gf (GListIAdv [x1,x2]) = mkApp (mkCId "BaseIAdv") [gf x1, gf x2]
  gf (GListIAdv (x:xs)) = mkApp (mkCId "ConsIAdv") [gf x, gf (GListIAdv xs)]
  fg t =
    GListIAdv (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseIAdv" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsIAdv" -> fg x1 : fgs x2


      _ -> error ("no ListIAdv " ++ show t)

instance Gf GListNP where
  gf (GListNP [x1,x2]) = mkApp (mkCId "BaseNP") [gf x1, gf x2]
  gf (GListNP (x:xs)) = mkApp (mkCId "ConsNP") [gf x, gf (GListNP xs)]
  fg t =
    GListNP (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseNP" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsNP" -> fg x1 : fgs x2


      _ -> error ("no ListNP " ++ show t)

instance Gf GListRS where
  gf (GListRS [x1,x2]) = mkApp (mkCId "BaseRS") [gf x1, gf x2]
  gf (GListRS (x:xs)) = mkApp (mkCId "ConsRS") [gf x, gf (GListRS xs)]
  fg t =
    GListRS (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseRS" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsRS" -> fg x1 : fgs x2


      _ -> error ("no ListRS " ++ show t)

instance Gf GListS where
  gf (GListS [x1,x2]) = mkApp (mkCId "BaseS") [gf x1, gf x2]
  gf (GListS (x:xs)) = mkApp (mkCId "ConsS") [gf x, gf (GListS xs)]
  fg t =
    GListS (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseS" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsS" -> fg x1 : fgs x2


      _ -> error ("no ListS " ++ show t)

instance Gf GMark where
  gf (Ga_Mark x1) = mkApp (mkCId "a_Mark") [gf x1]
  gf Gb_Mark = mkApp (mkCId "b_Mark") []
  gf Gh1_Mark = mkApp (mkCId "h1_Mark") []
  gf Gh2_Mark = mkApp (mkCId "h2_Mark") []
  gf Gi_Mark = mkApp (mkCId "i_Mark") []
  gf Gli_Mark = mkApp (mkCId "li_Mark") []
  gf Gp_Mark = mkApp (mkCId "p_Mark") []
  gf Gtable_Mark = mkApp (mkCId "table_Mark") []
  gf Gtd_Mark = mkApp (mkCId "td_Mark") []
  gf Gtr_Mark = mkApp (mkCId "tr_Mark") []
  gf Gul_Mark = mkApp (mkCId "ul_Mark") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "a_Mark" -> Ga_Mark (fg x1)
      Just (i,[]) | i == mkCId "b_Mark" -> Gb_Mark 
      Just (i,[]) | i == mkCId "h1_Mark" -> Gh1_Mark 
      Just (i,[]) | i == mkCId "h2_Mark" -> Gh2_Mark 
      Just (i,[]) | i == mkCId "i_Mark" -> Gi_Mark 
      Just (i,[]) | i == mkCId "li_Mark" -> Gli_Mark 
      Just (i,[]) | i == mkCId "p_Mark" -> Gp_Mark 
      Just (i,[]) | i == mkCId "table_Mark" -> Gtable_Mark 
      Just (i,[]) | i == mkCId "td_Mark" -> Gtd_Mark 
      Just (i,[]) | i == mkCId "tr_Mark" -> Gtr_Mark 
      Just (i,[]) | i == mkCId "ul_Mark" -> Gul_Mark 


      _ -> error ("no Mark " ++ show t)

instance Gf GMonth where
  gf Gapril_Month = mkApp (mkCId "april_Month") []
  gf Gaugust_Month = mkApp (mkCId "august_Month") []
  gf Gdecember_Month = mkApp (mkCId "december_Month") []
  gf Gfebruary_Month = mkApp (mkCId "february_Month") []
  gf Gjanuary_Month = mkApp (mkCId "january_Month") []
  gf Gjuly_Month = mkApp (mkCId "july_Month") []
  gf Gjune_Month = mkApp (mkCId "june_Month") []
  gf Gmarch_Month = mkApp (mkCId "march_Month") []
  gf Gmay_Month = mkApp (mkCId "may_Month") []
  gf Gnovember_Month = mkApp (mkCId "november_Month") []
  gf Goctober_Month = mkApp (mkCId "october_Month") []
  gf Gseptember_Month = mkApp (mkCId "september_Month") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "april_Month" -> Gapril_Month 
      Just (i,[]) | i == mkCId "august_Month" -> Gaugust_Month 
      Just (i,[]) | i == mkCId "december_Month" -> Gdecember_Month 
      Just (i,[]) | i == mkCId "february_Month" -> Gfebruary_Month 
      Just (i,[]) | i == mkCId "january_Month" -> Gjanuary_Month 
      Just (i,[]) | i == mkCId "july_Month" -> Gjuly_Month 
      Just (i,[]) | i == mkCId "june_Month" -> Gjune_Month 
      Just (i,[]) | i == mkCId "march_Month" -> Gmarch_Month 
      Just (i,[]) | i == mkCId "may_Month" -> Gmay_Month 
      Just (i,[]) | i == mkCId "november_Month" -> Gnovember_Month 
      Just (i,[]) | i == mkCId "october_Month" -> Goctober_Month 
      Just (i,[]) | i == mkCId "september_Month" -> Gseptember_Month 


      _ -> error ("no Month " ++ show t)

instance Gf GMonthday where
  gf (GintMonthday x1) = mkApp (mkCId "intMonthday") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "intMonthday" -> GintMonthday (fg x1)


      _ -> error ("no Monthday " ++ show t)

instance Gf GN where
  gf (GmonthN x1) = mkApp (mkCId "monthN") [gf x1]
  gf (GweekdayN x1) = mkApp (mkCId "weekdayN") [gf x1]
  gf (LexN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "monthN" -> GmonthN (fg x1)
      Just (i,[x1]) | i == mkCId "weekdayN" -> GweekdayN (fg x1)

      Just (i,[]) -> LexN (showCId i)
      _ -> error ("no N " ++ show t)

instance Gf GN2 where
  gf (GComplN3 x1 x2) = mkApp (mkCId "ComplN3") [gf x1, gf x2]
  gf (GUse2N3 x1) = mkApp (mkCId "Use2N3") [gf x1]
  gf (GUse3N3 x1) = mkApp (mkCId "Use3N3") [gf x1]
  gf (LexN2 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ComplN3" -> GComplN3 (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "Use2N3" -> GUse2N3 (fg x1)
      Just (i,[x1]) | i == mkCId "Use3N3" -> GUse3N3 (fg x1)

      Just (i,[]) -> LexN2 (showCId i)
      _ -> error ("no N2 " ++ show t)

instance Gf GN3 where
  gf (LexN3 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexN3 (showCId i)
      _ -> error ("no N3 " ++ show t)

instance Gf GNP where
  gf (GAdvNP x1 x2) = mkApp (mkCId "AdvNP") [gf x1, gf x2]
  gf (GConjNP x1 x2) = mkApp (mkCId "ConjNP") [gf x1, gf x2]
  gf (GCountNP x1 x2) = mkApp (mkCId "CountNP") [gf x1, gf x2]
  gf (GDetCN x1 x2) = mkApp (mkCId "DetCN") [gf x1, gf x2]
  gf (GDetNP x1) = mkApp (mkCId "DetNP") [gf x1]
  gf (GExtAdvNP x1 x2) = mkApp (mkCId "ExtAdvNP") [gf x1, gf x2]
  gf (GMarkupNP x1 x2) = mkApp (mkCId "MarkupNP") [gf x1, gf x2]
  gf (GMassNP x1) = mkApp (mkCId "MassNP") [gf x1]
  gf (GPPartNP x1 x2) = mkApp (mkCId "PPartNP") [gf x1, gf x2]
  gf (GPredetNP x1 x2) = mkApp (mkCId "PredetNP") [gf x1, gf x2]
  gf (GRelNP x1 x2) = mkApp (mkCId "RelNP") [gf x1, gf x2]
  gf (GSelfNP x1) = mkApp (mkCId "SelfNP") [gf x1]
  gf (GUsePN x1) = mkApp (mkCId "UsePN") [gf x1]
  gf (GUsePron x1) = mkApp (mkCId "UsePron") [gf x1]
  gf Geverybody_NP = mkApp (mkCId "everybody_NP") []
  gf Geverything_NP = mkApp (mkCId "everything_NP") []
  gf (GlanguageNP x1) = mkApp (mkCId "languageNP") [gf x1]
  gf (Gn_units_of_NP x1 x2 x3) = mkApp (mkCId "n_units_of_NP") [gf x1, gf x2, gf x3]
  gf Gnobody_NP = mkApp (mkCId "nobody_NP") []
  gf Gnothing_NP = mkApp (mkCId "nothing_NP") []
  gf Gsomebody_NP = mkApp (mkCId "somebody_NP") []
  gf Gsomething_NP = mkApp (mkCId "something_NP") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvNP" -> GAdvNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjNP" -> GConjNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "CountNP" -> GCountNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "DetCN" -> GDetCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "DetNP" -> GDetNP (fg x1)
      Just (i,[x1,x2]) | i == mkCId "ExtAdvNP" -> GExtAdvNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "MarkupNP" -> GMarkupNP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "MassNP" -> GMassNP (fg x1)
      Just (i,[x1,x2]) | i == mkCId "PPartNP" -> GPPartNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "PredetNP" -> GPredetNP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RelNP" -> GRelNP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "SelfNP" -> GSelfNP (fg x1)
      Just (i,[x1]) | i == mkCId "UsePN" -> GUsePN (fg x1)
      Just (i,[x1]) | i == mkCId "UsePron" -> GUsePron (fg x1)
      Just (i,[]) | i == mkCId "everybody_NP" -> Geverybody_NP 
      Just (i,[]) | i == mkCId "everything_NP" -> Geverything_NP 
      Just (i,[x1]) | i == mkCId "languageNP" -> GlanguageNP (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "n_units_of_NP" -> Gn_units_of_NP (fg x1) (fg x2) (fg x3)
      Just (i,[]) | i == mkCId "nobody_NP" -> Gnobody_NP 
      Just (i,[]) | i == mkCId "nothing_NP" -> Gnothing_NP 
      Just (i,[]) | i == mkCId "somebody_NP" -> Gsomebody_NP 
      Just (i,[]) | i == mkCId "something_NP" -> Gsomething_NP 


      _ -> error ("no NP " ++ show t)

instance Gf GNum where
  gf (GNumCard x1) = mkApp (mkCId "NumCard") [gf x1]
  gf GNumPl = mkApp (mkCId "NumPl") []
  gf GNumSg = mkApp (mkCId "NumSg") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "NumCard" -> GNumCard (fg x1)
      Just (i,[]) | i == mkCId "NumPl" -> GNumPl 
      Just (i,[]) | i == mkCId "NumSg" -> GNumSg 


      _ -> error ("no Num " ++ show t)

instance Gf GNumeral where
  gf (Gdigits2num x1) = mkApp (mkCId "digits2num") [gf x1]
  gf (Gnum x1) = mkApp (mkCId "num") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "digits2num" -> Gdigits2num (fg x1)
      Just (i,[x1]) | i == mkCId "num" -> Gnum (fg x1)


      _ -> error ("no Numeral " ++ show t)

instance Gf GOrd where
  gf (GOrdDigits x1) = mkApp (mkCId "OrdDigits") [gf x1]
  gf (GOrdNumeral x1) = mkApp (mkCId "OrdNumeral") [gf x1]
  gf (GOrdNumeralSuperl x1 x2) = mkApp (mkCId "OrdNumeralSuperl") [gf x1, gf x2]
  gf (GOrdSuperl x1) = mkApp (mkCId "OrdSuperl") [gf x1]
  gf Gleft_Ord = mkApp (mkCId "left_Ord") []
  gf Gright_Ord = mkApp (mkCId "right_Ord") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "OrdDigits" -> GOrdDigits (fg x1)
      Just (i,[x1]) | i == mkCId "OrdNumeral" -> GOrdNumeral (fg x1)
      Just (i,[x1,x2]) | i == mkCId "OrdNumeralSuperl" -> GOrdNumeralSuperl (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "OrdSuperl" -> GOrdSuperl (fg x1)
      Just (i,[]) | i == mkCId "left_Ord" -> Gleft_Ord 
      Just (i,[]) | i == mkCId "right_Ord" -> Gright_Ord 


      _ -> error ("no Ord " ++ show t)

instance Gf GPConj where
  gf GNoPConj = mkApp (mkCId "NoPConj") []
  gf (GPConjConj x1) = mkApp (mkCId "PConjConj") [gf x1]
  gf Gbut_PConj = mkApp (mkCId "but_PConj") []
  gf Gotherwise_PConj = mkApp (mkCId "otherwise_PConj") []
  gf Gtherefore_PConj = mkApp (mkCId "therefore_PConj") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "NoPConj" -> GNoPConj 
      Just (i,[x1]) | i == mkCId "PConjConj" -> GPConjConj (fg x1)
      Just (i,[]) | i == mkCId "but_PConj" -> Gbut_PConj 
      Just (i,[]) | i == mkCId "otherwise_PConj" -> Gotherwise_PConj 
      Just (i,[]) | i == mkCId "therefore_PConj" -> Gtherefore_PConj 


      _ -> error ("no PConj " ++ show t)

instance Gf GPN where
  gf (GmonthPN x1) = mkApp (mkCId "monthPN") [gf x1]
  gf (GweekdayPN x1) = mkApp (mkCId "weekdayPN") [gf x1]
  gf (LexPN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "monthPN" -> GmonthPN (fg x1)
      Just (i,[x1]) | i == mkCId "weekdayPN" -> GweekdayPN (fg x1)

      Just (i,[]) -> LexPN (showCId i)
      _ -> error ("no PN " ++ show t)

instance Gf GPhr where
  gf (GMarkupPhr x1 x2) = mkApp (mkCId "MarkupPhr") [gf x1, gf x2]
  gf (GPhrUtt x1 x2 x3) = mkApp (mkCId "PhrUtt") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "MarkupPhr" -> GMarkupPhr (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "PhrUtt" -> GPhrUtt (fg x1) (fg x2) (fg x3)


      _ -> error ("no Phr " ++ show t)

instance Gf GPol where
  gf GPNeg = mkApp (mkCId "PNeg") []
  gf GPPos = mkApp (mkCId "PPos") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "PNeg" -> GPNeg 
      Just (i,[]) | i == mkCId "PPos" -> GPPos 


      _ -> error ("no Pol " ++ show t)

instance Gf GPredet where
  gf Gall_Predet = mkApp (mkCId "all_Predet") []
  gf Gmost_Predet = mkApp (mkCId "most_Predet") []
  gf Gnot_Predet = mkApp (mkCId "not_Predet") []
  gf Gonly_Predet = mkApp (mkCId "only_Predet") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "all_Predet" -> Gall_Predet 
      Just (i,[]) | i == mkCId "most_Predet" -> Gmost_Predet 
      Just (i,[]) | i == mkCId "not_Predet" -> Gnot_Predet 
      Just (i,[]) | i == mkCId "only_Predet" -> Gonly_Predet 


      _ -> error ("no Predet " ++ show t)

instance Gf GPrep where
  gf Gabove_Prep = mkApp (mkCId "above_Prep") []
  gf Gafter_Prep = mkApp (mkCId "after_Prep") []
  gf Gbefore_Prep = mkApp (mkCId "before_Prep") []
  gf Gbehind_Prep = mkApp (mkCId "behind_Prep") []
  gf Gbetween_Prep = mkApp (mkCId "between_Prep") []
  gf Gby8agent_Prep = mkApp (mkCId "by8agent_Prep") []
  gf Gby8means_Prep = mkApp (mkCId "by8means_Prep") []
  gf Gduring_Prep = mkApp (mkCId "during_Prep") []
  gf Gexcept_Prep = mkApp (mkCId "except_Prep") []
  gf Gfor_Prep = mkApp (mkCId "for_Prep") []
  gf Gfrom_Prep = mkApp (mkCId "from_Prep") []
  gf Gin8front_Prep = mkApp (mkCId "in8front_Prep") []
  gf Gin_Prep = mkApp (mkCId "in_Prep") []
  gf Gon_Prep = mkApp (mkCId "on_Prep") []
  gf Gpart_Prep = mkApp (mkCId "part_Prep") []
  gf Gpossess_Prep = mkApp (mkCId "possess_Prep") []
  gf Gthrough_Prep = mkApp (mkCId "through_Prep") []
  gf Gto_Prep = mkApp (mkCId "to_Prep") []
  gf Gunder_Prep = mkApp (mkCId "under_Prep") []
  gf Gwith_Prep = mkApp (mkCId "with_Prep") []
  gf Gwithout_Prep = mkApp (mkCId "without_Prep") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "above_Prep" -> Gabove_Prep 
      Just (i,[]) | i == mkCId "after_Prep" -> Gafter_Prep 
      Just (i,[]) | i == mkCId "before_Prep" -> Gbefore_Prep 
      Just (i,[]) | i == mkCId "behind_Prep" -> Gbehind_Prep 
      Just (i,[]) | i == mkCId "between_Prep" -> Gbetween_Prep 
      Just (i,[]) | i == mkCId "by8agent_Prep" -> Gby8agent_Prep 
      Just (i,[]) | i == mkCId "by8means_Prep" -> Gby8means_Prep 
      Just (i,[]) | i == mkCId "during_Prep" -> Gduring_Prep 
      Just (i,[]) | i == mkCId "except_Prep" -> Gexcept_Prep 
      Just (i,[]) | i == mkCId "for_Prep" -> Gfor_Prep 
      Just (i,[]) | i == mkCId "from_Prep" -> Gfrom_Prep 
      Just (i,[]) | i == mkCId "in8front_Prep" -> Gin8front_Prep 
      Just (i,[]) | i == mkCId "in_Prep" -> Gin_Prep 
      Just (i,[]) | i == mkCId "on_Prep" -> Gon_Prep 
      Just (i,[]) | i == mkCId "part_Prep" -> Gpart_Prep 
      Just (i,[]) | i == mkCId "possess_Prep" -> Gpossess_Prep 
      Just (i,[]) | i == mkCId "through_Prep" -> Gthrough_Prep 
      Just (i,[]) | i == mkCId "to_Prep" -> Gto_Prep 
      Just (i,[]) | i == mkCId "under_Prep" -> Gunder_Prep 
      Just (i,[]) | i == mkCId "with_Prep" -> Gwith_Prep 
      Just (i,[]) | i == mkCId "without_Prep" -> Gwithout_Prep 


      _ -> error ("no Prep " ++ show t)

instance Gf GPron where
  gf Ghe_Pron = mkApp (mkCId "he_Pron") []
  gf Gi_Pron = mkApp (mkCId "i_Pron") []
  gf Git_Pron = mkApp (mkCId "it_Pron") []
  gf Gshe_Pron = mkApp (mkCId "she_Pron") []
  gf Gthey_Pron = mkApp (mkCId "they_Pron") []
  gf Gwe_Pron = mkApp (mkCId "we_Pron") []
  gf GyouPl_Pron = mkApp (mkCId "youPl_Pron") []
  gf GyouPol_Pron = mkApp (mkCId "youPol_Pron") []
  gf GyouSg_Pron = mkApp (mkCId "youSg_Pron") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "he_Pron" -> Ghe_Pron 
      Just (i,[]) | i == mkCId "i_Pron" -> Gi_Pron 
      Just (i,[]) | i == mkCId "it_Pron" -> Git_Pron 
      Just (i,[]) | i == mkCId "she_Pron" -> Gshe_Pron 
      Just (i,[]) | i == mkCId "they_Pron" -> Gthey_Pron 
      Just (i,[]) | i == mkCId "we_Pron" -> Gwe_Pron 
      Just (i,[]) | i == mkCId "youPl_Pron" -> GyouPl_Pron 
      Just (i,[]) | i == mkCId "youPol_Pron" -> GyouPol_Pron 
      Just (i,[]) | i == mkCId "youSg_Pron" -> GyouSg_Pron 


      _ -> error ("no Pron " ++ show t)

instance Gf GQCl where
  gf (GExistIP x1) = mkApp (mkCId "ExistIP") [gf x1]
  gf (GExistIPAdv x1 x2) = mkApp (mkCId "ExistIPAdv") [gf x1, gf x2]
  gf (GQuestCl x1) = mkApp (mkCId "QuestCl") [gf x1]
  gf (GQuestIAdv x1 x2) = mkApp (mkCId "QuestIAdv") [gf x1, gf x2]
  gf (GQuestIComp x1 x2) = mkApp (mkCId "QuestIComp") [gf x1, gf x2]
  gf (GQuestQVP x1 x2) = mkApp (mkCId "QuestQVP") [gf x1, gf x2]
  gf (GQuestSlash x1 x2) = mkApp (mkCId "QuestSlash") [gf x1, gf x2]
  gf (GQuestVP x1 x2) = mkApp (mkCId "QuestVP") [gf x1, gf x2]
  gf (Ghow_far_QCl x1) = mkApp (mkCId "how_far_QCl") [gf x1]
  gf (Ghow_old_QCl x1) = mkApp (mkCId "how_old_QCl") [gf x1]
  gf (Gwhat_name_QCl x1) = mkApp (mkCId "what_name_QCl") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ExistIP" -> GExistIP (fg x1)
      Just (i,[x1,x2]) | i == mkCId "ExistIPAdv" -> GExistIPAdv (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "QuestCl" -> GQuestCl (fg x1)
      Just (i,[x1,x2]) | i == mkCId "QuestIAdv" -> GQuestIAdv (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QuestIComp" -> GQuestIComp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QuestQVP" -> GQuestQVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QuestSlash" -> GQuestSlash (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "QuestVP" -> GQuestVP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "how_far_QCl" -> Ghow_far_QCl (fg x1)
      Just (i,[x1]) | i == mkCId "how_old_QCl" -> Ghow_old_QCl (fg x1)
      Just (i,[x1]) | i == mkCId "what_name_QCl" -> Gwhat_name_QCl (fg x1)


      _ -> error ("no QCl " ++ show t)

instance Gf GQS where
  gf (GUseQCl x1 x2 x3) = mkApp (mkCId "UseQCl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "UseQCl" -> GUseQCl (fg x1) (fg x2) (fg x3)


      _ -> error ("no QS " ++ show t)

instance Gf GQVP where
  gf (GAddAdvQVP x1 x2) = mkApp (mkCId "AddAdvQVP") [gf x1, gf x2]
  gf (GAdvQVP x1 x2) = mkApp (mkCId "AdvQVP") [gf x1, gf x2]
  gf (GComplSlashIP x1 x2) = mkApp (mkCId "ComplSlashIP") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AddAdvQVP" -> GAddAdvQVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AdvQVP" -> GAdvQVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplSlashIP" -> GComplSlashIP (fg x1) (fg x2)


      _ -> error ("no QVP " ++ show t)

instance Gf GQuant where
  gf GDefArt = mkApp (mkCId "DefArt") []
  gf GIndefArt = mkApp (mkCId "IndefArt") []
  gf (GPossPron x1) = mkApp (mkCId "PossPron") [gf x1]
  gf Gno_Quant = mkApp (mkCId "no_Quant") []
  gf Gthat_Quant = mkApp (mkCId "that_Quant") []
  gf Gthis_Quant = mkApp (mkCId "this_Quant") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "DefArt" -> GDefArt 
      Just (i,[]) | i == mkCId "IndefArt" -> GIndefArt 
      Just (i,[x1]) | i == mkCId "PossPron" -> GPossPron (fg x1)
      Just (i,[]) | i == mkCId "no_Quant" -> Gno_Quant 
      Just (i,[]) | i == mkCId "that_Quant" -> Gthat_Quant 
      Just (i,[]) | i == mkCId "this_Quant" -> Gthis_Quant 


      _ -> error ("no Quant " ++ show t)

instance Gf GRCl where
  gf (GRelCl x1) = mkApp (mkCId "RelCl") [gf x1]
  gf (GRelSlash x1 x2) = mkApp (mkCId "RelSlash") [gf x1, gf x2]
  gf (GRelVP x1 x2) = mkApp (mkCId "RelVP") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "RelCl" -> GRelCl (fg x1)
      Just (i,[x1,x2]) | i == mkCId "RelSlash" -> GRelSlash (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RelVP" -> GRelVP (fg x1) (fg x2)


      _ -> error ("no RCl " ++ show t)

instance Gf GRP where
  gf (GFunRP x1 x2 x3) = mkApp (mkCId "FunRP") [gf x1, gf x2, gf x3]
  gf GIdRP = mkApp (mkCId "IdRP") []

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "FunRP" -> GFunRP (fg x1) (fg x2) (fg x3)
      Just (i,[]) | i == mkCId "IdRP" -> GIdRP 


      _ -> error ("no RP " ++ show t)

instance Gf GRS where
  gf (GConjRS x1 x2) = mkApp (mkCId "ConjRS") [gf x1, gf x2]
  gf (GUseRCl x1 x2 x3) = mkApp (mkCId "UseRCl") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjRS" -> GConjRS (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "UseRCl" -> GUseRCl (fg x1) (fg x2) (fg x3)


      _ -> error ("no RS " ++ show t)

instance Gf GS where
  gf (GAdvS x1 x2) = mkApp (mkCId "AdvS") [gf x1, gf x2]
  gf (GConjS x1 x2) = mkApp (mkCId "ConjS") [gf x1, gf x2]
  gf (GExtAdvS x1 x2) = mkApp (mkCId "ExtAdvS") [gf x1, gf x2]
  gf (GMarkupS x1 x2) = mkApp (mkCId "MarkupS") [gf x1, gf x2]
  gf (GRelS x1 x2) = mkApp (mkCId "RelS") [gf x1, gf x2]
  gf (GSSubjS x1 x2 x3) = mkApp (mkCId "SSubjS") [gf x1, gf x2, gf x3]
  gf (GUseCl x1 x2 x3) = mkApp (mkCId "UseCl") [gf x1, gf x2, gf x3]
  gf (Gfew_X_short_of_Y x1 x2 x3) = mkApp (mkCId "few_X_short_of_Y") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvS" -> GAdvS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ConjS" -> GConjS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ExtAdvS" -> GExtAdvS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "MarkupS" -> GMarkupS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "RelS" -> GRelS (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "SSubjS" -> GSSubjS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "UseCl" -> GUseCl (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2,x3]) | i == mkCId "few_X_short_of_Y" -> Gfew_X_short_of_Y (fg x1) (fg x2) (fg x3)


      _ -> error ("no S " ++ show t)

instance Gf GSC where
  gf (GEmbedQS x1) = mkApp (mkCId "EmbedQS") [gf x1]
  gf (GEmbedS x1) = mkApp (mkCId "EmbedS") [gf x1]
  gf (GEmbedVP x1) = mkApp (mkCId "EmbedVP") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "EmbedQS" -> GEmbedQS (fg x1)
      Just (i,[x1]) | i == mkCId "EmbedS" -> GEmbedS (fg x1)
      Just (i,[x1]) | i == mkCId "EmbedVP" -> GEmbedVP (fg x1)


      _ -> error ("no SC " ++ show t)

instance Gf GSSlash where
  gf (GUseSlash x1 x2 x3) = mkApp (mkCId "UseSlash") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "UseSlash" -> GUseSlash (fg x1) (fg x2) (fg x3)


      _ -> error ("no SSlash " ++ show t)

instance Gf GSub10 where
  gf (Gdn10 x1) = mkApp (mkCId "dn10") [gf x1]
  gf (Gpot0 x1) = mkApp (mkCId "pot0") [gf x1]
  gf Gpot01 = mkApp (mkCId "pot01") []

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "dn10" -> Gdn10 (fg x1)
      Just (i,[x1]) | i == mkCId "pot0" -> Gpot0 (fg x1)
      Just (i,[]) | i == mkCId "pot01" -> Gpot01 


      _ -> error ("no Sub10 " ++ show t)

instance Gf GSub100 where
  gf (Gdn100 x1 x2) = mkApp (mkCId "dn100") [gf x1, gf x2]
  gf (Gpot0as1 x1) = mkApp (mkCId "pot0as1") [gf x1]
  gf (Gpot1 x1) = mkApp (mkCId "pot1") [gf x1]
  gf Gpot110 = mkApp (mkCId "pot110") []
  gf Gpot111 = mkApp (mkCId "pot111") []
  gf (Gpot1plus x1 x2) = mkApp (mkCId "pot1plus") [gf x1, gf x2]
  gf (Gpot1to19 x1) = mkApp (mkCId "pot1to19") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "dn100" -> Gdn100 (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "pot0as1" -> Gpot0as1 (fg x1)
      Just (i,[x1]) | i == mkCId "pot1" -> Gpot1 (fg x1)
      Just (i,[]) | i == mkCId "pot110" -> Gpot110 
      Just (i,[]) | i == mkCId "pot111" -> Gpot111 
      Just (i,[x1,x2]) | i == mkCId "pot1plus" -> Gpot1plus (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "pot1to19" -> Gpot1to19 (fg x1)


      _ -> error ("no Sub100 " ++ show t)

instance Gf GSub1000 where
  gf (Gdn1000 x1 x2 x3) = mkApp (mkCId "dn1000") [gf x1, gf x2, gf x3]
  gf (Gpot1as2 x1) = mkApp (mkCId "pot1as2") [gf x1]
  gf (Gpot2 x1) = mkApp (mkCId "pot2") [gf x1]
  gf (Gpot2plus x1 x2) = mkApp (mkCId "pot2plus") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "dn1000" -> Gdn1000 (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "pot1as2" -> Gpot1as2 (fg x1)
      Just (i,[x1]) | i == mkCId "pot2" -> Gpot2 (fg x1)
      Just (i,[x1,x2]) | i == mkCId "pot2plus" -> Gpot2plus (fg x1) (fg x2)


      _ -> error ("no Sub1000 " ++ show t)

instance Gf GSub1000000 where
  gf (Gdn1000000a x1 x2 x3 x4) = mkApp (mkCId "dn1000000a") [gf x1, gf x2, gf x3, gf x4]
  gf (Gdn1000000b x1 x2 x3 x4 x5) = mkApp (mkCId "dn1000000b") [gf x1, gf x2, gf x3, gf x4, gf x5]
  gf (Gdn1000000c x1 x2 x3 x4 x5 x6) = mkApp (mkCId "dn1000000c") [gf x1, gf x2, gf x3, gf x4, gf x5, gf x6]
  gf (Gpot2as3 x1) = mkApp (mkCId "pot2as3") [gf x1]
  gf (Gpot3 x1) = mkApp (mkCId "pot3") [gf x1]
  gf (Gpot3plus x1 x2) = mkApp (mkCId "pot3plus") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3,x4]) | i == mkCId "dn1000000a" -> Gdn1000000a (fg x1) (fg x2) (fg x3) (fg x4)
      Just (i,[x1,x2,x3,x4,x5]) | i == mkCId "dn1000000b" -> Gdn1000000b (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
      Just (i,[x1,x2,x3,x4,x5,x6]) | i == mkCId "dn1000000c" -> Gdn1000000c (fg x1) (fg x2) (fg x3) (fg x4) (fg x5) (fg x6)
      Just (i,[x1]) | i == mkCId "pot2as3" -> Gpot2as3 (fg x1)
      Just (i,[x1]) | i == mkCId "pot3" -> Gpot3 (fg x1)
      Just (i,[x1,x2]) | i == mkCId "pot3plus" -> Gpot3plus (fg x1) (fg x2)


      _ -> error ("no Sub1000000 " ++ show t)

instance Gf GSubj where
  gf Galthough_Subj = mkApp (mkCId "although_Subj") []
  gf Gbecause_Subj = mkApp (mkCId "because_Subj") []
  gf Gif_Subj = mkApp (mkCId "if_Subj") []
  gf Gthat_Subj = mkApp (mkCId "that_Subj") []
  gf Gwhen_Subj = mkApp (mkCId "when_Subj") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "although_Subj" -> Galthough_Subj 
      Just (i,[]) | i == mkCId "because_Subj" -> Gbecause_Subj 
      Just (i,[]) | i == mkCId "if_Subj" -> Gif_Subj 
      Just (i,[]) | i == mkCId "that_Subj" -> Gthat_Subj 
      Just (i,[]) | i == mkCId "when_Subj" -> Gwhen_Subj 


      _ -> error ("no Subj " ++ show t)

instance Gf GTag where
  gf (GMkTag x1) = mkApp (mkCId "MkTag") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "MkTag" -> GMkTag (fg x1)


      _ -> error ("no Tag " ++ show t)

instance Gf GTemp where
  gf (GTTAnt x1 x2) = mkApp (mkCId "TTAnt") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "TTAnt" -> GTTAnt (fg x1) (fg x2)


      _ -> error ("no Temp " ++ show t)

instance Gf GTense where
  gf GTCond = mkApp (mkCId "TCond") []
  gf GTFut = mkApp (mkCId "TFut") []
  gf GTPast = mkApp (mkCId "TPast") []
  gf GTPres = mkApp (mkCId "TPres") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "TCond" -> GTCond 
      Just (i,[]) | i == mkCId "TFut" -> GTFut 
      Just (i,[]) | i == mkCId "TPast" -> GTPast 
      Just (i,[]) | i == mkCId "TPres" -> GTPres 


      _ -> error ("no Tense " ++ show t)

instance Gf GText where
  gf (GMarkupText x1 x2) = mkApp (mkCId "MarkupText") [gf x1, gf x2]
  gf GTEmpty = mkApp (mkCId "TEmpty") []
  gf (GTExclMark x1 x2) = mkApp (mkCId "TExclMark") [gf x1, gf x2]
  gf (GTFullStop x1 x2) = mkApp (mkCId "TFullStop") [gf x1, gf x2]
  gf (GTQuestMark x1 x2) = mkApp (mkCId "TQuestMark") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "MarkupText" -> GMarkupText (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "TEmpty" -> GTEmpty 
      Just (i,[x1,x2]) | i == mkCId "TExclMark" -> GTExclMark (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "TFullStop" -> GTFullStop (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "TQuestMark" -> GTQuestMark (fg x1) (fg x2)


      _ -> error ("no Text " ++ show t)

instance Gf GTimeunit where
  gf Gday_Timeunit = mkApp (mkCId "day_Timeunit") []
  gf Ghour_Timeunit = mkApp (mkCId "hour_Timeunit") []
  gf Gminute_Timeunit = mkApp (mkCId "minute_Timeunit") []
  gf Gmonth_Timeunit = mkApp (mkCId "month_Timeunit") []
  gf Gsecond_Timeunit = mkApp (mkCId "second_Timeunit") []
  gf Gweek_Timeunit = mkApp (mkCId "week_Timeunit") []
  gf Gyear_Timeunit = mkApp (mkCId "year_Timeunit") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "day_Timeunit" -> Gday_Timeunit 
      Just (i,[]) | i == mkCId "hour_Timeunit" -> Ghour_Timeunit 
      Just (i,[]) | i == mkCId "minute_Timeunit" -> Gminute_Timeunit 
      Just (i,[]) | i == mkCId "month_Timeunit" -> Gmonth_Timeunit 
      Just (i,[]) | i == mkCId "second_Timeunit" -> Gsecond_Timeunit 
      Just (i,[]) | i == mkCId "week_Timeunit" -> Gweek_Timeunit 
      Just (i,[]) | i == mkCId "year_Timeunit" -> Gyear_Timeunit 


      _ -> error ("no Timeunit " ++ show t)

instance Gf GUtt where
  gf (GImpP3 x1 x2) = mkApp (mkCId "ImpP3") [gf x1, gf x2]
  gf (GImpPl1 x1) = mkApp (mkCId "ImpPl1") [gf x1]
  gf (GMarkupUtt x1 x2) = mkApp (mkCId "MarkupUtt") [gf x1, gf x2]
  gf (GUttAP x1) = mkApp (mkCId "UttAP") [gf x1]
  gf (GUttAdv x1) = mkApp (mkCId "UttAdv") [gf x1]
  gf (GUttCN x1) = mkApp (mkCId "UttCN") [gf x1]
  gf (GUttCard x1) = mkApp (mkCId "UttCard") [gf x1]
  gf (GUttIAdv x1) = mkApp (mkCId "UttIAdv") [gf x1]
  gf (GUttIP x1) = mkApp (mkCId "UttIP") [gf x1]
  gf (GUttImpPl x1 x2) = mkApp (mkCId "UttImpPl") [gf x1, gf x2]
  gf (GUttImpPol x1 x2) = mkApp (mkCId "UttImpPol") [gf x1, gf x2]
  gf (GUttImpSg x1 x2) = mkApp (mkCId "UttImpSg") [gf x1, gf x2]
  gf (GUttInterj x1) = mkApp (mkCId "UttInterj") [gf x1]
  gf (GUttNP x1) = mkApp (mkCId "UttNP") [gf x1]
  gf (GUttQS x1) = mkApp (mkCId "UttQS") [gf x1]
  gf (GUttS x1) = mkApp (mkCId "UttS") [gf x1]
  gf (GUttVP x1) = mkApp (mkCId "UttVP") [gf x1]
  gf Glanguage_title_Utt = mkApp (mkCId "language_title_Utt") []
  gf Gno_Utt = mkApp (mkCId "no_Utt") []
  gf Gyes_Utt = mkApp (mkCId "yes_Utt") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ImpP3" -> GImpP3 (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ImpPl1" -> GImpPl1 (fg x1)
      Just (i,[x1,x2]) | i == mkCId "MarkupUtt" -> GMarkupUtt (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UttAP" -> GUttAP (fg x1)
      Just (i,[x1]) | i == mkCId "UttAdv" -> GUttAdv (fg x1)
      Just (i,[x1]) | i == mkCId "UttCN" -> GUttCN (fg x1)
      Just (i,[x1]) | i == mkCId "UttCard" -> GUttCard (fg x1)
      Just (i,[x1]) | i == mkCId "UttIAdv" -> GUttIAdv (fg x1)
      Just (i,[x1]) | i == mkCId "UttIP" -> GUttIP (fg x1)
      Just (i,[x1,x2]) | i == mkCId "UttImpPl" -> GUttImpPl (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "UttImpPol" -> GUttImpPol (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "UttImpSg" -> GUttImpSg (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UttInterj" -> GUttInterj (fg x1)
      Just (i,[x1]) | i == mkCId "UttNP" -> GUttNP (fg x1)
      Just (i,[x1]) | i == mkCId "UttQS" -> GUttQS (fg x1)
      Just (i,[x1]) | i == mkCId "UttS" -> GUttS (fg x1)
      Just (i,[x1]) | i == mkCId "UttVP" -> GUttVP (fg x1)
      Just (i,[]) | i == mkCId "language_title_Utt" -> Glanguage_title_Utt 
      Just (i,[]) | i == mkCId "no_Utt" -> Gno_Utt 
      Just (i,[]) | i == mkCId "yes_Utt" -> Gyes_Utt 


      _ -> error ("no Utt " ++ show t)

instance Gf GV where
  gf (LexV x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexV (showCId i)
      _ -> error ("no V " ++ show t)

instance Gf GV2 where
  gf (LexV2 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexV2 (showCId i)
      _ -> error ("no V2 " ++ show t)

instance Gf GV2A where
  gf Gpaint_V2A = mkApp (mkCId "paint_V2A") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "paint_V2A" -> Gpaint_V2A 


      _ -> error ("no V2A " ++ show t)

instance Gf GV2Q where
  gf (LexV2Q x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexV2Q (showCId i)
      _ -> error ("no V2Q " ++ show t)

instance Gf GV2S where
  gf (LexV2S x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexV2S (showCId i)
      _ -> error ("no V2S " ++ show t)

instance Gf GV2V where
  gf (LexV2V x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexV2V (showCId i)
      _ -> error ("no V2V " ++ show t)

instance Gf GV3 where
  gf (LexV3 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexV3 (showCId i)
      _ -> error ("no V3 " ++ show t)

instance Gf GVA where
  gf Gbecome_VA = mkApp (mkCId "become_VA") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "become_VA" -> Gbecome_VA 


      _ -> error ("no VA " ++ show t)

instance Gf GVP where
  gf (GAdVVP x1 x2) = mkApp (mkCId "AdVVP") [gf x1, gf x2]
  gf (GAdvVP x1 x2) = mkApp (mkCId "AdvVP") [gf x1, gf x2]
  gf (GComplSlash x1 x2) = mkApp (mkCId "ComplSlash") [gf x1, gf x2]
  gf (GComplVA x1 x2) = mkApp (mkCId "ComplVA") [gf x1, gf x2]
  gf (GComplVQ x1 x2) = mkApp (mkCId "ComplVQ") [gf x1, gf x2]
  gf (GComplVS x1 x2) = mkApp (mkCId "ComplVS") [gf x1, gf x2]
  gf (GComplVV x1 x2) = mkApp (mkCId "ComplVV") [gf x1, gf x2]
  gf (GExtAdvVP x1 x2) = mkApp (mkCId "ExtAdvVP") [gf x1, gf x2]
  gf (GPassV2 x1) = mkApp (mkCId "PassV2") [gf x1]
  gf (GProgrVP x1) = mkApp (mkCId "ProgrVP") [gf x1]
  gf (GReflVP x1) = mkApp (mkCId "ReflVP") [gf x1]
  gf (GSelfAdVVP x1) = mkApp (mkCId "SelfAdVVP") [gf x1]
  gf (GSelfAdvVP x1) = mkApp (mkCId "SelfAdvVP") [gf x1]
  gf (GUseComp x1) = mkApp (mkCId "UseComp") [gf x1]
  gf GUseCopula = mkApp (mkCId "UseCopula") []
  gf (GUseV x1) = mkApp (mkCId "UseV") [gf x1]
  gf (Ghas_age_VP x1) = mkApp (mkCId "has_age_VP") [gf x1]
  gf Ghungry_VP = mkApp (mkCId "hungry_VP") []
  gf Gill_VP = mkApp (mkCId "ill_VP") []
  gf Gis_right_VP = mkApp (mkCId "is_right_VP") []
  gf Gis_wrong_VP = mkApp (mkCId "is_wrong_VP") []
  gf Gready_VP = mkApp (mkCId "ready_VP") []
  gf Gscared_VP = mkApp (mkCId "scared_VP") []
  gf Gthirsty_VP = mkApp (mkCId "thirsty_VP") []
  gf Gtired_VP = mkApp (mkCId "tired_VP") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdVVP" -> GAdVVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AdvVP" -> GAdvVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplSlash" -> GComplSlash (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplVA" -> GComplVA (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplVQ" -> GComplVQ (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplVS" -> GComplVS (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplVV" -> GComplVV (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ExtAdvVP" -> GExtAdvVP (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "PassV2" -> GPassV2 (fg x1)
      Just (i,[x1]) | i == mkCId "ProgrVP" -> GProgrVP (fg x1)
      Just (i,[x1]) | i == mkCId "ReflVP" -> GReflVP (fg x1)
      Just (i,[x1]) | i == mkCId "SelfAdVVP" -> GSelfAdVVP (fg x1)
      Just (i,[x1]) | i == mkCId "SelfAdvVP" -> GSelfAdvVP (fg x1)
      Just (i,[x1]) | i == mkCId "UseComp" -> GUseComp (fg x1)
      Just (i,[]) | i == mkCId "UseCopula" -> GUseCopula 
      Just (i,[x1]) | i == mkCId "UseV" -> GUseV (fg x1)
      Just (i,[x1]) | i == mkCId "has_age_VP" -> Ghas_age_VP (fg x1)
      Just (i,[]) | i == mkCId "hungry_VP" -> Ghungry_VP 
      Just (i,[]) | i == mkCId "ill_VP" -> Gill_VP 
      Just (i,[]) | i == mkCId "is_right_VP" -> Gis_right_VP 
      Just (i,[]) | i == mkCId "is_wrong_VP" -> Gis_wrong_VP 
      Just (i,[]) | i == mkCId "ready_VP" -> Gready_VP 
      Just (i,[]) | i == mkCId "scared_VP" -> Gscared_VP 
      Just (i,[]) | i == mkCId "thirsty_VP" -> Gthirsty_VP 
      Just (i,[]) | i == mkCId "tired_VP" -> Gtired_VP 


      _ -> error ("no VP " ++ show t)

instance Gf GVPSlash where
  gf (GAdVVPSlash x1 x2) = mkApp (mkCId "AdVVPSlash") [gf x1, gf x2]
  gf (GAdvVPSlash x1 x2) = mkApp (mkCId "AdvVPSlash") [gf x1, gf x2]
  gf (GSlash2V3 x1 x2) = mkApp (mkCId "Slash2V3") [gf x1, gf x2]
  gf (GSlash3V3 x1 x2) = mkApp (mkCId "Slash3V3") [gf x1, gf x2]
  gf (GSlashV2A x1 x2) = mkApp (mkCId "SlashV2A") [gf x1, gf x2]
  gf (GSlashV2Q x1 x2) = mkApp (mkCId "SlashV2Q") [gf x1, gf x2]
  gf (GSlashV2S x1 x2) = mkApp (mkCId "SlashV2S") [gf x1, gf x2]
  gf (GSlashV2V x1 x2) = mkApp (mkCId "SlashV2V") [gf x1, gf x2]
  gf (GSlashV2VNP x1 x2 x3) = mkApp (mkCId "SlashV2VNP") [gf x1, gf x2, gf x3]
  gf (GSlashV2a x1) = mkApp (mkCId "SlashV2a") [gf x1]
  gf (GSlashVV x1 x2) = mkApp (mkCId "SlashVV") [gf x1, gf x2]
  gf (GVPSlashPrep x1 x2) = mkApp (mkCId "VPSlashPrep") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdVVPSlash" -> GAdVVPSlash (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "AdvVPSlash" -> GAdvVPSlash (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Slash2V3" -> GSlash2V3 (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Slash3V3" -> GSlash3V3 (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SlashV2A" -> GSlashV2A (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SlashV2Q" -> GSlashV2Q (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SlashV2S" -> GSlashV2S (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "SlashV2V" -> GSlashV2V (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "SlashV2VNP" -> GSlashV2VNP (fg x1) (fg x2) (fg x3)
      Just (i,[x1]) | i == mkCId "SlashV2a" -> GSlashV2a (fg x1)
      Just (i,[x1,x2]) | i == mkCId "SlashVV" -> GSlashVV (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "VPSlashPrep" -> GVPSlashPrep (fg x1) (fg x2)


      _ -> error ("no VPSlash " ++ show t)

instance Gf GVQ where
  gf (LexVQ x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexVQ (showCId i)
      _ -> error ("no VQ " ++ show t)

instance Gf GVS where
  gf (LexVS x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexVS (showCId i)
      _ -> error ("no VS " ++ show t)

instance Gf GVV where
  gf Gcan8know_VV = mkApp (mkCId "can8know_VV") []
  gf Gcan_VV = mkApp (mkCId "can_VV") []
  gf Gmust_VV = mkApp (mkCId "must_VV") []
  gf Gwant_VV = mkApp (mkCId "want_VV") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "can8know_VV" -> Gcan8know_VV 
      Just (i,[]) | i == mkCId "can_VV" -> Gcan_VV 
      Just (i,[]) | i == mkCId "must_VV" -> Gmust_VV 
      Just (i,[]) | i == mkCId "want_VV" -> Gwant_VV 


      _ -> error ("no VV " ++ show t)

instance Gf GVoc where
  gf GNoVoc = mkApp (mkCId "NoVoc") []
  gf (GVocNP x1) = mkApp (mkCId "VocNP") [gf x1]
  gf Gplease_Voc = mkApp (mkCId "please_Voc") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "NoVoc" -> GNoVoc 
      Just (i,[x1]) | i == mkCId "VocNP" -> GVocNP (fg x1)
      Just (i,[]) | i == mkCId "please_Voc" -> Gplease_Voc 


      _ -> error ("no Voc " ++ show t)

instance Gf GWeekday where
  gf Gfriday_Weekday = mkApp (mkCId "friday_Weekday") []
  gf Gmonday_Weekday = mkApp (mkCId "monday_Weekday") []
  gf Gsaturday_Weekday = mkApp (mkCId "saturday_Weekday") []
  gf Gsunday_Weekday = mkApp (mkCId "sunday_Weekday") []
  gf Gthursday_Weekday = mkApp (mkCId "thursday_Weekday") []
  gf Gtuesday_Weekday = mkApp (mkCId "tuesday_Weekday") []
  gf Gwednesday_Weekday = mkApp (mkCId "wednesday_Weekday") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "friday_Weekday" -> Gfriday_Weekday 
      Just (i,[]) | i == mkCId "monday_Weekday" -> Gmonday_Weekday 
      Just (i,[]) | i == mkCId "saturday_Weekday" -> Gsaturday_Weekday 
      Just (i,[]) | i == mkCId "sunday_Weekday" -> Gsunday_Weekday 
      Just (i,[]) | i == mkCId "thursday_Weekday" -> Gthursday_Weekday 
      Just (i,[]) | i == mkCId "tuesday_Weekday" -> Gtuesday_Weekday 
      Just (i,[]) | i == mkCId "wednesday_Weekday" -> Gwednesday_Weekday 


      _ -> error ("no Weekday " ++ show t)

instance Gf GYear where
  gf (GintYear x1) = mkApp (mkCId "intYear") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "intYear" -> GintYear (fg x1)


      _ -> error ("no Year " ++ show t)


