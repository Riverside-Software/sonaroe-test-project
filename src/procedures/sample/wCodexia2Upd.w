&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ lib/comeon.v  }
{ lib/codexia.v }
{ lib/codexia.i }
{ lib/comeon.i  }
{ lib/appliged.i}
{ lib/defChampCodexia.v}

DEFINE VARIABLE cColonnes           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cValeurs            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOperateurs         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNiveau             AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cNomEcran           AS CHARACTER   NO-UNDO.

DEFINE VARIABLE h_wSuivi1           AS HANDLE      NO-UNDO.
DEFINE VARIABLE h_wDocu1            AS HANDLE      NO-UNDO.

DEFINE VARIABLE lRechEnCours        AS LOG         NO-UNDO INIT FALSE.

DEFINE VARIABLE iHeightViewer       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWidthViewer        AS INTEGER     NO-UNDO.

DEFINE VARIABLE clisteAppliGed      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE clisteLibAppliGed   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE h_dDocu1            AS HANDLE      NO-UNDO.
DEFINE VARIABLE iNiveauPrec         AS INTEGER     NO-UNDO.
DEFINE VARIABLE cChampsRupture2     AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-27 RECT-28 COMBO-BOX-Application ~
button-DelSsDossier button-AddSsDossier button-UpdSsDossier 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-Application Fi-Niveau2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "PhxXftr" wWin _INLINE
/* Actions: rtb/phxxftr1.p rtb/phxxftr2.p rtb/phxxftr3.p rtb/phxxftr4.p rtb/phxxftr5.p */
/* Pas de commentaires...
 */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-wWin 
       MENU-ITEM m_Modifier_ecran LABEL "Modifier ecran"
       MENU-ITEM m_Sauvegarde_des_modification LABEL "Sauvegarde des modifications"
       MENU-ITEM m_Annuler      LABEL "Annuler"       
       RULE
       MENU-ITEM m_Aligner_colonne LABEL "Aligner les champs par colonne"
       MENU-ITEM m_Aligner_ligne LABEL "Aligner les champs par ligne".


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_bCatalogue AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bCatalogue2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dCatalogue AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dCatalogue2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vCatalogue AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON button-AddSsDossier 
     IMAGE-UP FILE "img/add.bmp":U
     IMAGE-DOWN FILE "img/add.bmp":U NO-CONVERT-3D-COLORS
     LABEL "" 
     SIZE 4 BY .95.

DEFINE BUTTON button-DelSsDossier 
     IMAGE-UP FILE "img/deleterec.bmp":U
     IMAGE-DOWN FILE "img/deleterec.bmp":U NO-CONVERT-3D-COLORS
     LABEL "" 
     SIZE 4 BY 1.05.

DEFINE BUTTON button-UpdSsDossier 
     IMAGE-UP FILE "img/update.bmp":U
     IMAGE-DOWN FILE "img/update.bmp":U NO-CONVERT-3D-COLORS
     LABEL "" 
     SIZE 4 BY .95.

DEFINE BUTTON VALIDECHANGEMENT 
     IMAGE-UP FILE "img/ok.bmp":U
     IMAGE-INSENSITIVE FILE "img/okgris.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Valider le changement d'application" 
     SIZE 10 BY 1.

DEFINE VARIABLE COMBO-BOX-Application AS CHARACTER FORMAT "X(256)":U 
     LABEL "Application" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 77.4 BY 1 TOOLTIP "Selectionner l'application" NO-UNDO.

DEFINE VARIABLE Fi-Niveau2 AS CHARACTER FORMAT "X(256)":U INITIAL "Chemises" 
      VIEW-AS TEXT 
     SIZE 14.2 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 1.43.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17.4 BY 1.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     VALIDECHANGEMENT AT ROW 2.67 COL 91
     COMBO-BOX-Application AT ROW 2.67 COL 11.6 COLON-ALIGNED
     button-DelSsDossier AT ROW 16.19 COL 44.2
     button-AddSsDossier AT ROW 16.24 COL 38.8
     button-UpdSsDossier AT ROW 16.24 COL 49.6
     Fi-Niveau2 AT ROW 15.48 COL 38.6 NO-LABEL
     RECT-27 AT ROW 2.43 COL 1
     RECT-28 AT ROW 15.81 COL 37.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.4 BY 20.86.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Codexia"
         COLUMN             = 1.2
         ROW                = 1
         HEIGHT             = 20.05
         WIDTH              = 101.2
         MAX-HEIGHT         = 40.24
         MAX-WIDTH          = 230.4
         VIRTUAL-HEIGHT     = 40.24
         VIRTUAL-WIDTH      = 230.4
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         POPUP-MENU         = MENU POPUP-MENU-wWin:HANDLE
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:POPUP-MENU = MENU POPUP-MENU-wWin:HANDLE.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("img/icophx.ico":U) THEN
    MESSAGE "Unable to load icon: img/icophx.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN Fi-Niveau2 IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR BUTTON VALIDECHANGEMENT IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Codexia */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Codexia */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME button-AddSsDossier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL button-AddSsDossier wWin
ON CHOOSE OF button-AddSsDossier IN FRAME fMain
DO:
    RUN cdxMajDossier ( INPUT FALSE ) .
END. /*trigger*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME button-DelSsDossier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL button-DelSsDossier wWin
ON CHOOSE OF button-DelSsDossier IN FRAME fMain
DO:
    DEFINE VARIABLE lErreurLienDossier AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE lErreurDossier     AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE lErreur            AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE hHandle            AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cNoDossier         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cCodeCatalogue     AS CHARACTER  NO-UNDO.
    
    RUN CdxColValues IN h_dCatalogue2 ( OUTPUT cNoDossier , OUTPUT cCodeCatalogue ) .

    ASSIGN hHandle = DYNAMIC-FUNCTION('getASHandle':U IN h_dCatalogue2).  

    RUN PreTransactionDelete  IN hHandle ( INPUT cNoDossier     , 
                                           INPUT cCodeCatalogue ) .   /*IN h_dCatalogue2 on CodexiaL4G */

    RUN ReadErrorMes                     ( INPUT  hHandle       , 
                                           OUTPUT lErreur       ).
   
    { lib/ASDisconnect.i h_dCatalogue2 hHandle }

    IF NOT lErreur THEN
       DYNAMIC-FUNCTION('openQuery':U IN h_dCatalogue2) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME button-UpdSsDossier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL button-UpdSsDossier wWin
ON CHOOSE OF button-UpdSsDossier IN FRAME fMain
DO:
    RUN cdxMajDossier ( INPUT TRUE ) .
END. /*trigger*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Application
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Application wWin
ON VALUE-CHANGED OF COMBO-BOX-Application IN FRAME fMain /* Application */
DO:
  IF DYNAMIC-FUNCTION( "PhxGetSessionProperty", "CodeAppliGed" ) <> 
     ENTRY(LOOKUP(COMBO-BOX-Application:SCREEN-VALUE,clisteLibAppliGed),clisteAppliGed) THEN
          ASSIGN VALIDECHANGEMENT:SENSITIVE = TRUE. 
  ELSE 
          ASSIGN VALIDECHANGEMENT:SENSITIVE = FALSE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Aligner_colonne
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Aligner_colonne wWin
ON CHOOSE OF MENU-ITEM m_Aligner_colonne /* Aligner les champs par colonne */
DO:
      RUN cdxVertical.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Aligner_ligne
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Aligner_ligne wWin
ON CHOOSE OF MENU-ITEM m_Aligner_ligne /* Aligner les champs par ligne */
DO:
      RUN cdxHorizontal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Annuler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Annuler wWin
ON CHOOSE OF MENU-ITEM m_Annuler /* Annuler */
DO:
      RUN cdxAnnulerModifEcran.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Modifier_ecran
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Modifier_ecran wWin
ON CHOOSE OF MENU-ITEM m_Modifier_ecran /* Modifier ecran */
DO:
      RUN cdxModifEcran.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Sauvegarde_des_modification
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Sauvegarde_des_modification wWin
ON CHOOSE OF MENU-ITEM m_Sauvegarde_des_modification /* Sauvegarde des modifications */
DO:
      RUN cdxSvgEcran.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VALIDECHANGEMENT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VALIDECHANGEMENT wWin
ON CHOOSE OF VALIDECHANGEMENT IN FRAME fMain /* Valider le changement d'application */
DO:
  RUN cdxModifAppli.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

DELETE WIDGET-POOL "Attente" NO-ERROR.
/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'smt/vCatalogue.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable(None)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInityesDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vCatalogue ).
       RUN repositionObject IN h_vCatalogue ( 3.91 , 38.20 ) NO-ERROR.
       /* Size in AB:  ( 7.19 , 24.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FlatButtonsyesMenuyesShowBorderyesToolbaryesActionGroupsTableio,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrp,InstGrpSubModulesTableIOTypeUpdateSupportedLinksTableio-sourceToolbarBandsToolbarParentMenuToolbarAutoSizenoToolbarDrawDirectionhorizontalToolbarInitialStateLogicalObjectNameAutoResizeDisabledActionsHiddenActionsHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0EdgePixels2PanelTypeToolbarDeactivateTargetOnHidenoDisabledActionsNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar-2 ).
       RUN repositionObject IN h_dyntoolbar-2 ( 1.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar-2 ( 1.24 , 101.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN initPages ('1':U) NO-ERROR.

       /* Links to SmartDataViewer h_vCatalogue. */
       RUN addLink ( h_dCatalogue , 'Data':U , h_vCatalogue ).
       RUN addLink ( h_vCatalogue , 'Update':U , h_dCatalogue ).
       RUN addLink ( h_dyntoolbar-2 , 'TableIo':U , h_vCatalogue ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vCatalogue ,
             COMBO-BOX-Application:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'smt/dCatalogue.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceCodexiaAdmASUsePromptASInfoForeignFieldsRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeSTATELESSDestroyStatelessnoDisconnectAppServernoObjectNamedCatalogueUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dCatalogue ).
       RUN repositionObject IN h_dCatalogue ( 4.33 , 86.00 ) NO-ERROR.
       /* Size in AB:  ( 2.62 , 15.00 ) */

       RUN constructObject (
             INPUT  'smt/bCatalogue.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bCatalogue ).
       RUN repositionObject IN h_bCatalogue ( 3.86 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_bCatalogue ( 17.14 , 38.00 ) NO-ERROR.

       RUN constructObject (
             INPUT  'smt/dCatalogue.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceCodexiaAdmASUsePromptASInfoForeignFieldsCatalogue.CodeSociete,CodeSociete,Catalogue.CodeEtab,CodeEtab,Catalogue.CodeAgence,CodeAgence,Catalogue.CodeAppliGed,CodeAppliGed,Catalogue.CodeParent,CodeCatalogueRowsToBatch200CheckCurrentChangedyesRebuildOnReposnoServerOperatingModeNONEDestroyStatelessnoDisconnectAppServernoObjectNamedCatalogueUpdateFromSourcenoToggleDataTargetsyesOpenOnInityesPromptOnDeleteyesPromptColumns(NONE)':U ,
             OUTPUT h_dCatalogue2 ).
       RUN repositionObject IN h_dCatalogue2 ( 6.95 , 86.00 ) NO-ERROR.
       /* Size in AB:  ( 2.38 , 15.00 ) */

       RUN constructObject (
             INPUT  'smt/bCatalogue.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'ScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesDataSourceNamesUpdateTargetNamesLogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_bCatalogue2 ).
       RUN repositionObject IN h_bCatalogue2 ( 15.76 , 59.00 ) NO-ERROR.
       RUN resizeObject IN h_bCatalogue2 ( 5.24 , 42.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_bCatalogue. */
       RUN addLink ( h_dCatalogue , 'Data':U , h_bCatalogue ).

       /* Links to SmartDataObject h_dCatalogue2. */
       RUN addLink ( h_dCatalogue , 'Data':U , h_dCatalogue2 ).

       /* Links to SmartDataBrowser h_bCatalogue2. */
       RUN addLink ( h_dCatalogue2 , 'Data':U , h_bCatalogue2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_bCatalogue ,
             COMBO-BOX-Application:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_bCatalogue2 ,
             h_vCatalogue , 'AFTER':U ).
    END. /* Page 1 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CdxAfficheTitre wWin 
PROCEDURE CdxAfficheTitre :
/*------------------------------------------------------------------------------
  Purpose: Affiche le titre de la fenetre    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cTitre AS CHAR NO-UNDO.

cTitre = "Codexia - " + ttAppliGed.LibAppliGed.

IF lRechEnCours THEN
    /* cTitre = cTitre + " [Recherche en cours]". */
    cTitre = cTitre + " " + DYNAMIC-FUNCTION("phxGetTraduc", INPUT "[Recherche en cours]", 
                                                             INPUT "wTCodexia").



ASSIGN {&WINDOW-NAME}:TITLE = cTitre.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cdxAnnulerModifEcran wWin 
PROCEDURE cdxAnnulerModifEcran :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF ttAppliGed.AffDossier = 1 THEN
 DO:
    RUN cdxInitAffichage.
    PUBLISH 'cdxAfficheViewer':U .
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cdxBrowseLabelformat wWin 
PROCEDURE cdxBrowseLabelformat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pcListeChampBrowse  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcListeLabelBrowse  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pcListeFormatBrowse AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ph_dCatalogue       AS HANDLE NO-UNDO.


DEFINE VARIABLE i           AS INTEGER    NO-UNDO.
DEFINE VARIABLE cChamp      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDataType   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFormatZone AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iFormat     AS INTEGER    NO-UNDO.

DO i = 1 TO NUM-ENTRIES(pcListeChampBrowse) :
    /* modification du label si libelle renseigne */
    IF ENTRY(i,pcListeLabelBrowse) <> "" THEN
        DYNAMIC-FUNCTION('assignColumnLabel':U IN ph_dCatalogue,
                            INPUT ENTRY(i,pcListeChampBrowse),
                            INPUT ENTRY(i,pcListeLabelBrowse)).

    /* format */
    ASSIGN cChamp      = "Catalogue." + ENTRY(i,pcListeChampBrowse) 
           cFormatZone = "" .
    cDataType = DYNAMIC-FUNCTION('columnProps':U IN ph_dCatalogue,
                                  INPUT cChamp,
                                  INPUT 'DataType').
    CASE ENTRY(2,cDataType,CHR(4)) :
        WHEN "character" THEN 
            /* Porte le format a x(80) lorsque par defaut il est a plus de 100 */
            ASSIGN  iFormat    =  IF INT(ENTRY(i,pcListeFormatBrowse)) > 100 THEN 80 ELSE INT(ENTRY(i,pcListeFormatBrowse))
                    cFormatZone = "x(" + STRING(iFormat)  + ")".
        
        WHEN "integer" OR WHEN "decimal" THEN
            RUN ref/CalculFormat.p (INPUT ENTRY(i,pcListeFormatBrowse) , OUTPUT cFormatZone).
        
        WHEN "date" THEN 
            ASSIGN cFormatZone = "99/99/9999"  .
    END CASE.

    IF cFormatZone <> "" THEN
        DYNAMIC-FUNCTION('assignColumnFormat':U IN ph_dCatalogue,
                         INPUT ENTRY(i,pcListeChampBrowse),
                         INPUT cFormatZone).

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CdxCreateBrowse wWin 
PROCEDURE CdxCreateBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pcListeChamps  AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER piBrowseCol    AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER piBrowseRow    AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER piBrowseWidth  AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER piBrowseHeight AS DECIMAL    NO-UNDO.

DEFINE VARIABLE hBrowse AS HANDLE     NO-UNDO.

  IF VALID-HANDLE(h_bCatalogue) THEN 
        RUN destroyObject IN h_bCatalogue. 
 
  RUN constructObject (
         INPUT  'smt/bCatalogue.w':U ,
         INPUT  FRAME fMain:HANDLE ,
         INPUT  'ScrollRemotenoDataSourceNamesUpdateTargetNamesHideOnInitnoDisableOnInitnoObjectLayout':U ,
         OUTPUT h_bCatalogue ).

  /* Links to SmartDataBrowser h_bCatalogue. */
  RUN addLink ( h_dCatalogue , 'Data':U , h_bCatalogue ).

  /*
  RUN repositionObject IN h_bCatalogue ( 3.86 , 1.00 ) NO-ERROR.
 /* RUN resizeObject IN h_bCatalogue ( 10 , 41 ) NO-ERROR.  */
  RUN resizeObject IN h_bCatalogue ( 11 , 35 ) NO-ERROR. */

  DYNAMIC-FUNCTION('setDisplayedFields':U IN h_bCatalogue, INPUT pcListeChamps ).

  IF (ttAppliGed.AffDossier EQ 1) THEN DO:
     IF ((piBrowseWidth  NE ?) AND (piBrowseWidth  NE 0)  AND
         (piBrowseHeight NE ?) AND (piBrowseHeight NE 0)) THEN DO:
          RUN repositionObject IN h_bcatalogue (piBrowseRow   , piBrowseCol)   NO-ERROR.
          RUN resizeObject     IN h_bcatalogue (piBrowseHeight, piBrowseWidth) NO-ERROR.
     END.
     ELSE DO:
         RUN repositionObject IN h_bcatalogue ( 1.00 , 3.91 )   NO-ERROR.
         RUN resizeObject     IN h_bcatalogue ( 5.62 , 30.6 ) NO-ERROR.
     END.
  END.

 /* resize par defaut sur la largeur de la  window */ 
 /* IF ttAppliGed.AffDossier  = 1 THEN DO:    
       ASSIGN hBrowse = DYNAMIC-FUNCTION('getbrowsehandle':U IN h_bCatalogue).
       IF piBrowseWidth  <> ? AND piBrowseWidth  <> 0 AND
          piBrowseHeight <> ? AND piBrowseHeight <> 0 THEN DO :                         
           ASSIGN hBrowse:WIDTH  = piBrowseWidth
                  hBrowse:HEIGHT = piBrowseHeight
                  hBrowse:COL    = piBrowseCol
                  hBrowse:ROW    = piBrowseRow .            
       END.
       ELSE DO :  /* par defaut */
           ASSIGN hBrowse:WIDTH  = 32.8 
                  hBrowse:HEIGHT = 12.60   
                  hBrowse:COL    = 1
                  hBrowse:ROW    = 1 .  

       END.
  END.*/
  
  RUN initializeObject IN h_bCatalogue.  
  RUN viewObject IN h_bCatalogue.

     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cdxCreateBrowse2 wWin 
PROCEDURE cdxCreateBrowse2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pcListeChamps  AS CHAR       NO-UNDO.
DEFINE INPUT PARAMETER piBrowseCol    AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER piBrowseRow    AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER piBrowseWidth  AS DECIMAL    NO-UNDO.
DEFINE INPUT PARAMETER piBrowseHeight AS DECIMAL    NO-UNDO.

DEFINE VARIABLE hBrowse AS HANDLE     NO-UNDO.
DEFINE VARIABLE cBreakBy AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSection AS CHARACTER   NO-UNDO.


  IF VALID-HANDLE(h_bCatalogue2) THEN 
        RUN destroyObject IN h_bCatalogue2. 
 
  RUN constructObject (
         INPUT  'smt/bCatalogue.w':U ,
         INPUT  FRAME fMain:HANDLE ,
         INPUT  'ScrollRemotenoDataSourceNamesUpdateTargetNamesHideOnInitnoDisableOnInitnoObjectLayout':U ,
         OUTPUT h_bCatalogue2 ).
 
  /* Links to SmartDataBrowser h_bCatalogue. */
  RUN addLink ( h_dCatalogue2 , 'Data':U , h_bCatalogue2 ).

  ASSIGN cSection = "wCodexia_" + cCodeAppliGed .
  RUN ref/Dllini3.p ( INPUT "LECTURE"         , INPUT ""                ,
                      INPUT cSection          , INPUT "TRIBROWSE2"      ,
                      INPUT-OUTPUT cBreakBy   , INPUT cComApplication   ,
                      INPUT "*"               , INPUT "*"               ).

  IF cBreakBy <> "" THEN DO :
      DYNAMIC-FUNCTION('setQuerySort':U  IN h_dCatalogue2, INPUT cBreakBy ).
      DYNAMIC-FUNCTION('openQuery':U     IN h_dCatalogue2                 ).
  END.

  DYNAMIC-FUNCTION('setDisplayedFields':U IN h_bCatalogue2, INPUT pcListeChamps ).

  IF (ttAppliGed.AffDossier EQ 1) THEN DO:
     IF ((piBrowseWidth  NE ?) AND (piBrowseWidth NE 0)   AND
         (piBrowseHeight NE ?) AND (piBrowseHeight NE 0)) THEN DO:
          RUN repositionObject IN h_bcatalogue2 (piBrowseRow, piBrowseCol) NO-ERROR.
          RUN resizeObject     IN h_bcatalogue2 (piBrowseHeight, piBrowseWidth) NO-ERROR.
     END.
     ELSE DO:
         RUN repositionObject IN h_bcatalogue2 ( 15.81  , 55.00 ) NO-ERROR.
         RUN resizeObject IN h_bcatalogue2     ( 5.00 , 45.00 ) NO-ERROR.
     END.
  END.

/*   RUN repositionObject IN h_bCatalogue2 ( 15.81 , 55.0  ) NO-ERROR.
  RUN resizeObject     IN h_bCatalogue2 (  5.00 , 45.00) NO-ERROR.
  
  IF ttAppliGed.AffDossier  = 1 THEN
  DO:
       ASSIGN hBrowse = DYNAMIC-FUNCTION('getbrowsehandle':U IN h_bCatalogue2).
       IF piBrowseWidth  <> ? AND piBrowseWidth  <> 0 AND
          piBrowseHeight <> ? AND piBrowseHeight <> 0 THEN DO :
              ASSIGN hBrowse:WIDTH  = piBrowseWidth
                     hBrowse:HEIGHT = piBrowseHeight
                     hBrowse:COL    = piBrowseCol
                     hBrowse:ROW    = piBrowseRow
                  . 
       END.     
       ELSE DO : 
             /* par defaut */
           ASSIGN hBrowse:WIDTH  = 45  /*51*/
                  hBrowse:HEIGHT = 5  
                  hBrowse:COL    = 55
                  hBrowse:ROW    = 1 .   
       END.
  END.*/

  RUN initializeObject IN h_bCatalogue2.  
  RUN viewObject IN h_bCatalogue2.

     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cdxCreateViewer wWin 
PROCEDURE cdxCreateViewer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER piViewerCol AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER piViewerRow AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER piViewerWidth  AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER piViewerHeight AS DECIMAL NO-UNDO.

    IF VALID-HANDLE(h_vCatalogue) THEN RUN destroyObject IN h_vCatalogue . 

    RUN constructObject (
        INPUT  'smt/vCatalogue.w':U ,
        INPUT  FRAME fMain:HANDLE ,
        INPUT  'ConfSupyesDataSourceNamesUpdateTargetNamesHideOnInityesDisableOnInityesObjectLayout':U ,
       OUTPUT h_vCatalogue ).

    /* Links to SmartDataViewer h_vCatalogue. */
    RUN addLink ( h_dCatalogue , 'Data':U , h_vCatalogue ).
    RUN addLink ( h_vCatalogue , 'Update':U , h_dCatalogue ).
    RUN addLink ( h_dyntoolbar-2 , 'TableIo':U , h_vCatalogue ).

    RUN initializeObject IN h_vCatalogue.
/*    IF piViewerCol <> 0 AND piViewerRow <> 0 THEN
       RUN repositionObject IN h_vCatalogue (piViewerRow , piViewerCol) NO-ERROR.
    ELSE
       RUN repositionObject IN h_vCatalogue ( 3.91 , 28.20 ) NO-ERROR.
       /*RUN repositionObject IN h_vCatalogue ( 4.10 , 34.00 ) NO-ERROR.*/
*/
       
   IF ((piViewerWidth NE ?) AND (piViewerWidth NE 0) AND
       (piViewerHeight NE ?) AND (piViewerHeight NE 0)) THEN DO:
       RUN repositionObject IN h_vCatalogue (piViewerRow   , piViewerCol) NO-ERROR.
       RUN resizeObject     IN h_vCatalogue (piViewerHeight, piViewerWidth) NO-ERROR.
   END.
   ELSE DO:
       IF VALID-HANDLE (h_vCatalogue) THEN RUN repositionObject IN h_vCatalogue ( 3.91 , 28.20 ) NO-ERROR.
       IF VALID-HANDLE (h_vCatalogue) THEN RUN resizeObject     IN h_vCatalogue ( 12.5 , 66.00 ) NO-ERROR.
   END.

   RUN viewObject IN h_vCatalogue.       


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cdxHorizontal wWin 
PROCEDURE cdxHorizontal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF ttAppliGed.AffDossier = 1 THEN
DO:
    /* viewer */
    PUBLISH "cdxAlignHorizontal".
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cdxInitAffichage wWin 
PROCEDURE cdxInitAffichage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cListeChampBrowse   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListeLabelBrowse   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListeFormatBrowse  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iViewerCol          AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iViewerRow          AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iViewerWidth        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iViewerHeight       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iBrowseCol          AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iBrowseRow          AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iBrowseWidth        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iBrowseHeight       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iBrowseCol2         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iBrowseRow2         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iBrowseWidth2       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iBrowseHeight2      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cTri                AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cChamp              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i                   AS INTEGER    NO-UNDO.
DEFINE VARIABLE cSection            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cChampCle2          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cChampCle3          AS CHARACTER  NO-UNDO.


RUN CdxAfficheTitre.

   
RUN ref/cdxEcranPrincipal.p (INPUT cComApplication ,
                           INPUT DYNAMIC-FUNCTION("PhxGetSessionProperty","CodeAppliGed")   ,
                          OUTPUT cNomEcran ).


/*ASSIGN cNomEcran = SUBSTRING(cNomEcran, INDEX(cNomEcran, '/' ) + 1 , LENGTH(cNomEcran) - INDEX(cNomEcran, '/' )).*/

/* recupere  la position des objets */
RUN cdxPositionObjet IN  h_dCatalogue (INPUT         "LECTURE"     , 
                                     INPUT-OUTPUT  iViewerCol    ,
                                     INPUT-OUTPUT  iViewerRow    ,
                                     INPUT-OUTPUT  iViewerWidth  ,
                                     INPUT-OUTPUT  iViewerHeight ,
                                     INPUT-OUTPUT  iBrowseCol    ,
                                     INPUT-OUTPUT  iBrowseRow    , 
                                     INPUT-OUTPUT  iBrowseWidth  ,
                                     INPUT-OUTPUT  iBrowseHeight ) .


RUN cdxPositionObjet2 IN  h_dCatalogue (INPUT         "LECTURE"     ,                                         
                                     INPUT-OUTPUT  iBrowseCol2    ,
                                     INPUT-OUTPUT  iBrowseRow2   , 
                                     INPUT-OUTPUT  iBrowseWidth2 ,
                                     INPUT-OUTPUT  iBrowseHeight2 ) .


/*-------- Affiche le browse ------------- */
/* recupere les champs,labels et formats du browse codexia et mise et creation du browse */
RUN ref/cdxChampBrowseCodexia.p (INPUT  cCodeAppliGed     ,
                           INPUT  cNomEcran         ,
                           OUTPUT cListeChampBrowse ,
                           OUTPUT cListeLabelBrowse ,
                           OUTPUT cListeFormatBrowse).



IF cListeChampBrowse <> "" THEN 
RUN cdxBrowseLabelformat (INPUT cListeChampBrowse ,
                          INPUT cListeLabelBrowse ,
                          INPUT cListeFormatBrowse,
                          INPUT h_dCatalogue        ) . 
ELSE
ASSIGN cListeChampBrowse = "CodeCatalogue,LibCatalogue".



/* Creation du browse */
RUN CdxCreateBrowse(INPUT cListeChampBrowse,
                    INPUT iBrowseCol       ,
                    INPUT iBrowseRow       ,
                    INPUT iBrowseWidth     ,
                    INPUT iBrowseHeight    ).


/*-------- Affiche le browse ------------- */
/* recupere les champs,labels et formats du browse codexia et mise et creation du browse */
RUN ref/cdxChampBrowseCodexia.p (INPUT  cCodeAppliGed     ,
                           INPUT  "b2-" + cNomEcran         ,
                           OUTPUT cListeChampBrowse ,
                           OUTPUT cListeLabelBrowse ,
                           OUTPUT cListeFormatBrowse).



IF cListeChampBrowse <> "" THEN 
    RUN cdxBrowseLabelformat (INPUT cListeChampBrowse ,
                              INPUT cListeLabelBrowse ,
                              INPUT cListeFormatBrowse,
                              INPUT h_dCatalogue2        ) .
ELSE ASSIGN cListeChampBrowse = "CodeCatalogue,LibCatalogue".   



IF NUM-ENTRIES (cListeChampBrowse)  > 0 THEN DO:      
    DO i = 1 TO  NUM-ENTRIES (cListeChampBrowse) :
            cChamp = ENTRY(i,cListeChampBrowse).
            cTri =  "BY Catalogue." + cChamp .
            IF cChamp BEGINS "DescD"  THEN  cTri = cTri + " DESCENDING".
            DYNAMIC-FUNCTION('setQuerySort':U  IN h_dCatalogue2, INPUT cTri ).  
    END.
END.
DYNAMIC-FUNCTION('openQuery':U IN  h_dCatalogue2).

/* Création du browse */
RUN CdxCreateBrowse2(INPUT cListeChampBrowse,
                  INPUT iBrowseCol2       ,
                  INPUT iBrowseRow2       ,
                  INPUT iBrowseWidth2     ,
                  INPUT iBrowseHeight2    ).




/*---------- affichage du viewer -------------*/
CASE ttAppliGed.AffDossier :
  WHEN 1 THEN  /* si presentation tableau */
  DO:
      RUN cdxCreateViewer (INPUT iViewerCol ,
                           INPUT iViewerRow,
                           INPUT iViewerWidth,
                           INPUT iViewerHeight).
      PUBLISH 'cdxAfficheViewer':U .
      PUBLISH 'cdxFillInCombo':U .
      PUBLISH 'cdxOrdreTabulation':U .
      RUN viewObject   IN h_vCatalogue.
      RUN enableObject IN h_vCatalogue.
  END.
  WHEN 2 THEN  /* presentation uniquement du browse */
  DO:
    RUN DisableObject IN h_vCatalogue. 
    RUN hideObject IN h_vCatalogue.
  END.
END CASE.



/*........Affichage des titres des panels de bouton du niveau 2 et niveau 3....*/
/*........En fonction du parametrage des cle, recuperer les libelle des........*/
/*........niveau 2 et 3 dans le parametrage des champs metier..................*/

/* Recherche de la valeur des cle 2 et 3 dans le cfginireg */
ASSIGN cSection = "wCodexia_" + cCodeAppliGed .

RUN ref/Dllini3.p ( INPUT "LECTURE"         , INPUT ""                ,
                    INPUT cSection          , INPUT "NI2"             ,
                    INPUT-OUTPUT cChampCle2 , INPUT cComApplication   ,
                    INPUT "*"               , INPUT "*"               ).

FIND FIRST ttChampAppliGed 
     WHERE ttChampAppliGed.CodeAppliGed = cCodeAppliGed
       AND ttChampAppliGed.NomChamp     = cChampCle2
     NO-LOCK NO-ERROR.
    
IF AVAILABLE ttChampAppliGed THEN Fi-Niveau2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ttChampAppliGed.Libelle.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CdxListeDocuments wWin 
PROCEDURE CdxListeDocuments :
/*------------------------------------------------------------------------------
  Purpose: lance la fenêtre de liste des documents    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iPageEnCours    AS INTEGER NO-UNDO.
DEFINE VARIABLE iNiveau         AS INTEGER NO-UNDO.

    /* ---- Recherche du niveau du BROWSE ---- */
    IF VALID-HANDLE(FOCUS) THEN DO:
            CASE INT(FOCUS:HANDLE) :
                WHEN INT(DYNAMIC-FUNCTION('getBrowseHandle':U IN h_bCatalogue )) THEN iNiveau = 1 .
                WHEN INT(DYNAMIC-FUNCTION('getBrowseHandle':U IN h_bCatalogue2)) THEN iNiveau = 2 .
                OTHERWISE iNiveau = 1 .
            END.
        END.
        ELSE ASSIGN iNiveau = 1.

    IF NOT VALID-HANDLE(h_wDocu1) THEN DO: 
        SESSION:SET-WAIT-STATE("GENERAL").
        { lib/f-attente.i &message = "Ouverture de la liste des documents..." &TypeDattente = "Codexia"}

        RUN constructObject (
                INPUT  'smt/wDocu1.w':U ,
                INPUT  {&WINDOW-NAME} ,
                INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
                OUTPUT h_wDocu1 ).

        iPageEnCours = DYNAMIC-FUNCTION('getCurrentPage':U).
        
                /* Bug #1324 : suppression du lien Data dans wDocu1, recree par initializeObject */
                /* Correction pas belle du tout, a virer des que possible (du genre migration ADM2) */
        RUN CdxRecupHandleSDO IN h_wDocu1  (OUTPUT h_dDocu1) .
        RUN removeLink IN h_wDocu1 (h_wDocu1, "DATA", h_dDocu1).
        CASE iNiveau :
            WHEN 1 THEN RUN addLink ( h_dCatalogue   ,'Data' , h_wDocu1 ).
            WHEN 2 THEN RUN addLink ( h_dCatalogue2  ,'Data' , h_wDocu1 ).
        END CASE.
        
        RUN initializeObject IN h_wDocu1.
        
        IF iPageEnCours > 0 THEN
            RUN removePageNTarget IN THIS-PROCEDURE (h_wDocu1, iPageEnCours).
        
        
        CASE iNiveau :
            WHEN 1 THEN PUBLISH 'DataAvailable' FROM h_dCatalogue   ( INPUT 'DIFFERENT' ).
            WHEN 2 THEN PUBLISH 'DataAvailable' FROM h_dCatalogue2  ( INPUT 'DIFFERENT' ).
        END CASE.

        CASE iNiveau :
            WHEN 1 THEN RUN CdxPublieNoDossier IN h_bCatalogue.
            WHEN 2 THEN RUN CdxPublieNoDossier IN h_bCatalogue2.
        END CASE.
        
        SESSION:SET-WAIT-STATE ("").
        DELETE WIDGET-POOL "attente" NO-ERROR.
    END.
    ELSE DO :
        /* ---- On supprime le link passthrough entre SDOdocu1 et WindwDocu1 ---- */
        RUN CdxRecupHandleSDO IN h_wDocu1  (OUTPUT h_dDocu1) .
        DYNAMIC-FUNCTION('SetForeignFields':U IN h_dDocu1 , INPUT '' ) .
        RUN RemoveLink IN h_wDocu1 ( h_wDocu1 ,'Data' , h_dDocu1 ) .
        
        /* ---- On supprime le link data entre SDOCatalogue et WindWdocu1 ---- */
        CASE iNiveauPrec :
            WHEN 1 THEN RUN RemoveLink ( h_dCatalogue  ,'Data' , h_wDocu1 ) .
            WHEN 2 THEN RUN RemoveLink ( h_dCatalogue2 ,'Data' , h_wDocu1 ). 
        END CASE.

        /* ---- En fonction du nouveau niveau on recre les liens data ---- */
        CASE iNiveau :
            WHEN 1 THEN RUN addLink ( h_dCatalogue   ,'Data' , h_wDocu1 ).
            WHEN 2 THEN RUN addLink ( h_dCatalogue2  ,'Data' , h_wDocu1 ).
        END CASE.

        RUN AddLink IN h_wDocu1 ( h_wDocu1 ,'Data' , h_dDocu1 ) .
        DYNAMIC-FUNCTION('SetForeignFields':U IN h_dDocu1 
                 , INPUT "LienDossier.CodeSociete,CodeSociete,LienDossier.CodeEtab,CodeEtab,LienDossier.CodeAgence,CodeAgence,LienDossier.CodeAppliGed,CodeAppliGed,LienDossier.NoDossier,NoDossier" ) .
        
        CASE iNiveau :
            WHEN 1 THEN PUBLISH 'DataAvailable' FROM h_dCatalogue   ( INPUT 'DIFFERENT' ).
            WHEN 2 THEN PUBLISH 'DataAvailable' FROM h_dCatalogue2  ( INPUT 'DIFFERENT' ).
        END CASE.

        CASE iNiveau :
            WHEN 1 THEN RUN CdxPublieNoDossier IN h_bCatalogue.
            WHEN 2 THEN RUN CdxPublieNoDossier IN h_bCatalogue2.
        END CASE.

        RUN viewObject IN h_wDocu1.
    END.
         
    ASSIGN iNiveauPrec = iNiveau.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cdxMajDossier wWin 
PROCEDURE cdxMajDossier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  pcMaj   AS LOGICAL    NO-UNDO.

    DEFINE VARIABLE hHandle         AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cColonnes       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cValeurs        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cOperateurs     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cListeModif     AS CHARACTER  NO-UNDO INIT "".
    DEFINE VARIABLE lStatut         AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cLibCatalogue   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSection        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cCle            AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSpeParameter   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cValeurCle2     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cChampCle2      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cListeParam     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cNoDossier      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cValeurCle1     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cChampCle1      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE i               AS INTEGER    NO-UNDO.
    /* Permet la gestion d'un ecran <> entre creation et modification */
    DEFINE VARIABLE cPrefixe        AS CHARACTER  NO-UNDO INIT "g2-".


    IF pcMaj AND DYNAMIC-FUNCTION('columnValue':U IN h_dCatalogue2, INPUT "NoDossier" ) = ? THEN
        RETURN .

    /* Recherche de la valeur de la cle dans le cfginireg */
    ASSIGN cSection = "wCodexia_" + cCodeAppliGed .

    RUN ref/Dllini3.p ( INPUT "LECTURE"         ,
                    INPUT ""                ,
                    INPUT cSection          ,
                    INPUT "KY"              ,
                    INPUT-OUTPUT cChampCle1 ,
                    INPUT cComApplication   ,
                    INPUT "*"               ,
                    INPUT "*"               ).
    RUN ref/Dllini3.p ( INPUT "LECTURE"         ,
                    INPUT ""                ,
                    INPUT cSection          ,
                    INPUT "NI2"             ,
                    INPUT-OUTPUT cChampCle2 ,
                    INPUT cComApplication   ,
                    INPUT "*"               ,
                    INPUT "*"               ).
    
    RUN cdxRecupCode IN h_vCatalogue ( INPUT  cChampCle1    , 
                                       OUTPUT cLibCatalogue ,
                                       OUTPUT cValeurCle1   ).

    IF pcMaj THEN DO :
        ASSIGN cPrefixe = "g2m-".
        DO i=1 TO 2 :       
            RUN ref/cdxChampRechSsDos.p ( INPUT cCodeAppliGed,
                                      INPUT cPrefixe + cNomEcran, 
                                     OUTPUT TABLE ttChampCodexia).
        
            FOR EACH ttChampCodexia NO-LOCK BREAK BY ttChampCodexia.NomChamp :
                IF FIRST ( ttChampCodexia.NomChamp) THEN 
                   ASSIGN cColonnes = ttChampCodexia.NomChamp 
                          cValeurs  = DYNAMIC-FUNCTION('columnValue':U IN h_dCatalogue2, 
                                                       INPUT ttChampCodexia.NomChamp ) .
                ELSE 
                   ASSIGN cColonnes = cColonnes + CHR(1) + ttChampCodexia.NomChamp 
                          cValeurs  = cValeurs  + CHR(1) + 
                                      DYNAMIC-FUNCTION('columnValue':U IN h_dCatalogue2, 
                                                       INPUT ttChampCodexia.NomChamp ).
            END.
            IF  cColonnes = ""  THEN   cPrefixe = "g2-" .
                        ELSE i = 2.                                                          
        END.
    END.
    ELSE ASSIGN cColonnes = '' cValeurs = '' .
  

    RUN smt/gUpdateSsDos.w ( INPUT INT(DYNAMIC-FUNCTION('columnValue':U IN h_dCatalogue2,INPUT "nodossier" )) ,
                             INPUT     DYNAMIC-FUNCTION('columnValue':U IN h_dCatalogue ,INPUT "CodeCatalogue" )  ,
                             INPUT        cPrefixe + cNomEcran ,
                             INPUT        pcMaj         ,
                             OUTPUT       cValeurCle2   ,
                             INPUT-OUTPUT cColonnes    ,
                             INPUT-OUTPUT cValeurs     ,
                             INPUT-OUTPUT cOperateurs  ,
                             OUTPUT       cListeModif  ,
                             OUTPUT       lStatut      ).
  
    IF cValeurCle1 <> "" AND cValeurCle2 <> "" THEN DO :

        IF pcMaj THEN DO :
            ASSIGN hHandle = DYNAMIC-FUNCTION('getASHandle':U IN h_dCatalogue2).  

            RUN CdxTransactionUpdate IN hHandle 
                ( INPUT DYNAMIC-FUNCTION('columnValue':U IN h_dCatalogue2, INPUT "NoDossier" )  ,
                  INPUT cColonnes                                                               ,
                  INPUT cValeurs                                                                ) .

            { lib/ASDisconnect.i h_dCatalogue2 hHandle }
        END.
        ELSE DO :
            ASSIGN cCle          = "KY"  + CHR(1) + cChampCle1 + CHR(1) + cValeurCle1 + CHR(1) 
                                 + "NI2" + CHR(1) + cChampCle2 + CHR(1) + cValeurCle2 
                   cSpeParameter = cChampCle1     + "=" + cValeurCle1   + CHR(1)
                                 + cChampCle2     + "=" + cValeurCle2   + CHR(1)
                                 + "LibCatalogue" + "=" + clibCatalogue + CHR(1)
                                 + "EstParent"    + "=" + "False"       .
            
            DO i = 1 TO NUM-ENTRIES(cColonnes,CHR(1)) :
               ASSIGN cSpeParameter = cSpeParameter + CHR(1) 
                                    + ENTRY( i , cColonnes , CHR(1)  ) + "=" + ENTRY( i , cValeurs , CHR(1)) .
            END.
            
            RUN spe/fnDossier.p (  INPUT "XMLSelisa"   , 
                                   INPUT cCle          ,
                                   INPUT cSpeParameter ,
                                   OUTPUT cNoDossier    ) .
        END.
        
        ASSIGN hHandle = DYNAMIC-FUNCTION('getASHandle':U IN h_dCatalogue2).  

        RUN CdxUpdateParent IN hHandle 
            ( INPUT (IF pcMaj THEN DYNAMIC-FUNCTION('columnValue':U IN h_dCatalogue2, INPUT "NoDossier") ELSE cNoDossier )).

        { lib/ASDisconnect.i h_dCatalogue2 hHandle }

        DYNAMIC-FUNCTION('openQuery':U IN h_dCatalogue2) . 
    END.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cdxModifAppli wWin 
PROCEDURE cdxModifAppli :
/**
 * Changement d'application GED : verification des droits et rechargement si OK
 */
 
    DEFINE VARIABLE hwChoixUtilisateur_Rq AS HANDLE      NO-UNDO.
    DEFINE VARIABLE hApsv AS HANDLE      NO-UNDO.
    DEFINE VARIABLE lOK   AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE cGroupe AS CHARACTER   NO-UNDO.

    IF ((COMBO-BOX-Application:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE ?)
        AND (COMBO-BOX-Application:SCREEN-VALUE NE "")
        AND (DYNAMIC-FUNCTION("PhxGetSessionProperty", "CodeAppliGed") NE ENTRY(LOOKUP(COMBO-BOX-Application:SCREEN-VALUE, clisteLibAppliGed), clisteAppliGed)))
    THEN DO:
        RUN appserverconnect(INPUT 'codexiaL4G', ?, ?, OUTPUT hApsv).
        RUN ref/apsv/changeAppliGed.p ON hApsv (INPUT ENTRY(LOOKUP(COMBO-BOX-Application:SCREEN-VALUE, clisteLibAppliGed), clisteAppliGed),
                                                OUTPUT cGroupe,
                                                OUTPUT lOK).
        RUN appServerDisconnect(INPUT 'CodexiaL4G').

        IF lOK THEN DO:
            {phxset 'ComGroupe' cGroupe}.
            {phxset 'CodeAppliGed' ENTRY(LOOKUP(COMBO-BOX-Application:SCREEN-VALUE,cListeLibAppliGed),cListeAppliGed)}.
             cCodeAppliged = DYNAMIC-FUNCTION("PhxGetSessionProperty","CodeAppliGed") .
            APPLY "CLOSE":U TO THIS-PROCEDURE.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            MESSAGE "Acces refuse a l'application"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cdxModifEcran wWin 
PROCEDURE cdxModifEcran :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE hBrowse AS HANDLE     NO-UNDO.
  

IF ttAppliGed.AffDossier = 1 THEN DO:

    /* viewer et Browses */
    PUBLISH "cdxModifierEcran".

    /*/* Browse */
    ASSIGN hBrowse = DYNAMIC-FUNCTION('getbrowsehandle':U IN h_bCatalogue)
           hBrowse:SELECTABLE = TRUE
           hBrowse:MOVABLE    = TRUE
           hBrowse:RESIZABLE  = TRUE.  

     /* Browse */
    ASSIGN hBrowse = DYNAMIC-FUNCTION('getbrowsehandle':U IN h_bCatalogue2)
           hBrowse:SELECTABLE = TRUE
           hBrowse:MOVABLE    = TRUE
           hBrowse:RESIZABLE  = TRUE.  */

END.
    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CdxRecherche wWin 
PROCEDURE CdxRecherche :
/*------------------------------------------------------------------------------
  Purpose: lance la dialog de construction du filtre    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cListeChangements AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCompteur         AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCptLst           AS INTEGER    NO-UNDO.
DEFINE VARIABLE lStatut           AS LOGICAL    NO-UNDO.

DEFINE VARIABLE cColonnesNiv1     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cColonnesNiv2     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cValeursNiv1      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cValeursNiv2      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOperateursNiv1   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOperateursNiv2   AS CHARACTER   NO-UNDO.

RUN smt/gRechCatalogue.w (INPUT-OUTPUT  cColonnes           ,
                          INPUT-OUTPUT  cValeurs            ,
                          INPUT-OUTPUT  cOperateurs         ,
                          INPUT-OUTPUT  cNiveau             ,
                          OUTPUT        cListeChangements   ,
                          OUTPUT        lStatut             ).

SESSION:SET-WAIT-STATE("GENERAL":U).

IF LENGTH(ENTRY(1,cListeChangements,CHR(11))) <> 0 THEN DO :
    DO iCompteur = 1 TO NUM-ENTRIES(ENTRY(1,cListeChangements,CHR(11)),CHR(3)) :
        CASE INT(ENTRY(3, ENTRY(iCompteur,cListeChangements,CHR(3)), CHR(1))) :
            WHEN 1 THEN DO :
                DYNAMIC-FUNCTION('removeQuerySelection':U IN h_dCatalogue,
                           INPUT ENTRY(1, ENTRY(iCompteur,cListeChangements,CHR(3)), CHR(1)),
                           INPUT ENTRY(2, ENTRY(iCompteur,cListeChangements,CHR(3)), CHR(1)) ).
            END.
            WHEN 2 THEN DO:
                DYNAMIC-FUNCTION('removeQuerySelection':U IN h_dCatalogue2,
                           INPUT ENTRY(1, ENTRY(iCompteur,cListeChangements,CHR(3)), CHR(1)),
                           INPUT ENTRY(2, ENTRY(iCompteur,cListeChangements,CHR(3)), CHR(1)) ).
            END.
        END CASE.
    END.
END.

IF cValeurs = "RAZ" THEN DO:
    IF VALID-HANDLE(h_dCatalogue2) THEN
        DYNAMIC-FUNCTION('removeQuerySelection':U IN h_dCatalogue2 ,
                   INPUT cColonnes,
                   INPUT cOperateurs).
    IF VALID-HANDLE(h_dCatalogue) THEN
        DYNAMIC-FUNCTION('removeQuerySelection':U IN h_dCatalogue,
                   INPUT cColonnes,
                   INPUT cOperateurs).

    ASSIGN  cColonnes   = ""
            cValeurs    = ""
            cOperateurs = ""
            cNiveau     = ""  .
END.
ELSE DO:
    /* On reconstruit une liste pour chaque niveau */
    DO iCptLst = 1 TO NUM-ENTRIES(cColonnes) :
        CASE INT(ENTRY(iCptLst, cNiveau, CHR(1))) :
            WHEN 1 THEN DO :
                  IF cColonnesNiv1 = '' 
                THEN ASSIGN cColonnesNiv1 = ENTRY(iCptLst, cColonnes).
                ELSE ASSIGN cColonnesNiv1 = cColonnesNiv1 + ',' + ENTRY(iCptLst, cColonnes) .
                  IF cValeursNiv1 = '' 
                THEN ASSIGN cValeursNiv1 = ENTRY(iCptLst, cValeurs , CHR(1) ).
                ELSE ASSIGN cValeursNiv1 = cValeursNiv1 + CHR(1) + ENTRY(iCptLst, cValeurs, CHR(1) ) .
                  IF cOperateursNiv1 = '' 
                THEN ASSIGN cOperateursNiv1 = ENTRY(iCptLst, cOperateurs).
                ELSE ASSIGN cOperateursNiv1 = cOperateursNiv1 + ',' + ENTRY(iCptLst, cOperateurs) .
            END.
            WHEN 2 THEN DO :
                  IF cColonnesNiv2 = '' 
                THEN ASSIGN cColonnesNiv2 = ENTRY(iCptLst, cColonnes).
                ELSE ASSIGN cColonnesNiv2 = cColonnesNiv2 + ',' + ENTRY(iCptLst, cColonnes) .
                  IF cValeursNiv2 = '' 
                THEN ASSIGN cValeursNiv2 = ENTRY(iCptLst, cValeurs , CHR(1)).
                ELSE ASSIGN cValeursNiv2 = cValeursNiv2 + CHR(1) + ENTRY(iCptLst, cValeurs, CHR(1) ) .
                  IF cOperateursNiv2 = '' 
                THEN ASSIGN cOperateursNiv2 = ENTRY(iCptLst, cOperateurs).
                ELSE ASSIGN cOperateursNiv2 = cOperateursNiv2 + ',' + ENTRY(iCptLst, cOperateurs) .
            END.
        END CASE.
    END.

    IF VALID-HANDLE(h_dCatalogue) THEN 
        DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dCatalogue,
                   INPUT cColonnesNiv1   ,
                   INPUT cValeursNiv1    ,
                   INPUT cOperateursNiv1 ).

    IF VALID-HANDLE(h_dCatalogue2) THEN
        DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dCatalogue2,
                   INPUT cColonnesNiv2   ,
                   INPUT cValeursNiv2    ,
                   INPUT cOperateursNiv2 ).
    
END.

IF lStatut THEN DO :
    IF VALID-HANDLE(h_dCatalogue2) THEN DYNAMIC-FUNCTION('openQuery':U IN h_dCatalogue2).
    IF VALID-HANDLE(h_dCatalogue)  THEN DYNAMIC-FUNCTION('openQuery':U IN h_dCatalogue).
END.

lRechEnCours = (cColonnes <> ""). 
RUN CdxPublieNoDossier IN h_bCatalogue2.

PUBLISH "CdxapplyEntry".

RUN cdxAfficheTitre.

SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cdxSuiviDossier wWin 
PROCEDURE cdxSuiviDossier :
/*------------------------------------------------------------------------------
  Purpose: lance la fenêtre de suivi de dossier    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iPageEnCours AS INT NO-UNDO.

IF NOT VALID-HANDLE(h_wSuivi1) THEN DO:    
    SESSION:SET-WAIT-STATE("GENERAL":U).
    RUN constructObject (
             INPUT  'smt/wSuivi1.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_wSuivi1 ).
    iPageEnCours = DYNAMIC-FUNCTION('getCurrentPage':U).
    
    RUN addLink (h_dCatalogue ,'Data':U , h_wSuivi1 ).

    RUN initializeObject IN h_wSuivi1.
    IF iPageEnCours > 0 THEN
        RUN removePageNTarget IN THIS-PROCEDURE (h_wSuivi1, iPageEnCours).
    SESSION:SET-WAIT-STATE ("").
END.
ELSE
    RUN viewObject IN h_wSuivi1.

 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cdxSvgEcran wWin 
PROCEDURE cdxSvgEcran :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iViewerCol    AS DEC    NO-UNDO.
DEFINE VARIABLE iViewerRow    AS DEC    NO-UNDO.
DEFINE VARIABLE iViewerWidth  AS DEC    NO-UNDO.
DEFINE VARIABLE iViewerHeight AS DEC    NO-UNDO.
DEFINE VARIABLE iBrowseCol    AS DEC    NO-UNDO.
DEFINE VARIABLE iBrowseRow    AS DEC    NO-UNDO.
DEFINE VARIABLE iBrowseWidth  AS DEC    NO-UNDO.
DEFINE VARIABLE iBrowseHeight AS DEC    NO-UNDO.
DEFINE VARIABLE hBrowse       AS HANDLE NO-UNDO.
DEFINE VARIABLE iBrowseCol2    AS DEC    NO-UNDO.
DEFINE VARIABLE iBrowseRow2    AS DEC    NO-UNDO.
DEFINE VARIABLE iBrowseWidth2  AS DEC    NO-UNDO.
DEFINE VARIABLE iBrowseHeight2 AS DEC    NO-UNDO.
DEFINE VARIABLE hBrowse2       AS HANDLE NO-UNDO.
DEFINE VARIABLE lPositionObjet AS LOGICAL  INIT TRUE  NO-UNDO.


/* si affichage du viewer */
IF ttAppliGed.AffDossier = 1 THEN
DO:
    RUN cdxSvgEcran in h_bCatalogue.
    RUN cdxSvgEcran in h_bCatalogue2.


    ASSIGN  iViewerCol    = DYNAMIC-FUNCTION('getCol':U    IN h_vCatalogue)
            iViewerRow    = DYNAMIC-FUNCTION('getRow':U    IN h_vCatalogue)
            iViewerWidth  = DYNAMIC-FUNCTION('getWidth':U  IN h_vCatalogue)
            iViewerHeight = DYNAMIC-FUNCTION('getHeight':U IN h_vCatalogue)
            hBrowse       = DYNAMIC-FUNCTION('getbrowsehandle':U IN h_bCatalogue)
            iBrowseWidth  = DYNAMIC-FUNCTION('getWidth':U  IN h_bCatalogue)
            iBrowseHeight = DYNAMIC-FUNCTION('getHeight':U IN h_bCatalogue)
            iBrowseCol    = DYNAMIC-FUNCTION('getCol':U    IN h_bCatalogue)
            iBrowseRow    = DYNAMIC-FUNCTION('getRow':U    IN h_bCatalogue)
            hBrowse2       = DYNAMIC-FUNCTION('getbrowsehandle':U IN h_bCatalogue2)
            iBrowseWidth2  = DYNAMIC-FUNCTION('getWidth':U  IN h_bCatalogue2)
            iBrowseHeight2 = DYNAMIC-FUNCTION('getHeight':U IN h_bCatalogue2)
            iBrowseCol2    = DYNAMIC-FUNCTION('getCol':U    IN h_bCatalogue2)
            iBrowseRow2    = DYNAMIC-FUNCTION('getRow':U    IN h_bCatalogue2)
           . 
    /*
            iBrowseWidth  = hBrowse:WIDTH
        iBrowseHeight = hBrowse:HEIGHT
        iBrowseCol    = hBrowse:COL
        iBrowseRow    = hBrowse:ROW
            iBrowseWidth2  = hBrowse2:WIDTH
            iBrowseHeight2 = hBrowse2:HEIGHT
            iBrowseCol2    = hBrowse2:COL
            iBrowseRow2    = hBrowse2:ROW
    */


   /*------------- Contrôle position objet-----------------------*/
    /* Si viewer positionne sur tool bar ou combo application */
    /*IF iViewerRow < 3.86 THEN 
        ASSIGN lPositionObjet = FALSE .
    
    ELSE 
    /* si browse positionne sur viewer */
        IF ( iBrowseCol >= iViewerCol  AND
             iBrowseCol <= (iViewerCol  + iViewerWidth) )
           AND
           /* le 2.86 correspond au 3.86 - 1 cad (row du viewer - row du browse) */
           ( iBrowseRow >= iViewerRow - 2.86 AND
             iBrowseRow <= (iViewerRow - 2.86 + iViewerHeight) )
        THEN
            ASSIGN lPositionObjet = FALSE .

    ELSE 
        IF ( iBrowseCol + iBrowseWidth >= iViewerCol  AND
             iBrowseCol + iBrowseWidth <= (iViewerCol  + iViewerWidth) )
           AND
           ( iBrowseRow >= iViewerRow - 2.86 AND
             iBrowseRow <= (iViewerRow - 2.86 + iViewerHeight) )
        THEN
            ASSIGN lPositionObjet = FALSE .

     ELSE 
        IF ( iBrowseCol + iBrowseWidth >= iViewerCol  AND
             iBrowseCol + iBrowseWidth <= (iViewerCol  + iViewerWidth) )
           AND
           ( iBrowseRow + iBrowseHeight >= iViewerRow - 2.86 AND
             iBrowseRow + iBrowseHeight <= (iViewerRow - 2.86 + iViewerHeight) )
        THEN
            ASSIGN lPositionObjet = FALSE .

     ELSE 
        IF ( iBrowseCol  >= iViewerCol  AND
             iBrowseCol  <= (iViewerCol  + iViewerWidth) )
           AND
           ( iBrowseRow + iBrowseHeight >= iViewerRow - 2.86 AND
             iBrowseRow + iBrowseHeight <= (iViewerRow - 2.86 + iViewerHeight) )
        THEN
            ASSIGN lPositionObjet = FALSE .

     /* si viewer positionne sur le browse */
     ELSE 
        IF ( iViewerCol >= iBrowseCol  AND
             iViewerCol <= (iBrowseCol  + iBrowseWidth) )
           AND
           ( iViewerRow - 2.86 >= iBrowseRow  AND
             iViewerRow - 2.86 <= (iBrowseRow + iBrowseHeight) )
        THEN
            ASSIGN lPositionObjet = FALSE .

     ELSE 
        IF ( iViewerCol + iViewerWidth >= iBrowseCol  AND
             iViewerCol + iViewerWidth <= (iBrowseCol  + iBrowseWidth) )
           AND
           ( iViewerRow - 2.86 >= iBrowseRow  AND
             iViewerRow - 2.86 <= (iBrowseRow + iBrowseHeight) )
        THEN
            ASSIGN lPositionObjet = FALSE .

     ELSE 
        IF ( iViewerCol + iViewerWidth >= iBrowseCol  AND
             iViewerCol + iViewerWidth <= (iBrowseCol  + iBrowseWidth) )
           AND
           ( iViewerRow - 2.86 + iViewerHeight >= iBrowseRow  AND
             iViewerRow - 2.86 + iViewerHeight <= (iBrowseRow + iBrowseHeight) )
        THEN
            ASSIGN lPositionObjet = FALSE .

      ELSE 
        IF ( iViewerCol  >= iBrowseCol  AND
             iViewerCol  <= (iBrowseCol  + iBrowseWidth) )
           AND
           ( iViewerRow - 2.86 + iViewerHeight >= iBrowseRow  AND
             iViewerRow - 2.86 + iViewerHeight <= (iBrowseRow + iBrowseHeight) )
        THEN
            ASSIGN lPositionObjet = FALSE .
   */
/* true */
    IF lPositionObjet   THEN DO:
        /* sauvgegarde des position champ Ecran et position des objets */
        PUBLISH "CdxUPDATEEcran" .
        RUN cdxPositionObjet IN  h_dCatalogue ( INPUT        "ECRITURE"     , 
                                                INPUT-OUTPUT iViewerCol     ,
                                                INPUT-OUTPUT iViewerRow     ,
                                                INPUT-OUTPU  iViewerWidth   ,
                                                INPUT-OUTPUT iViewerHeight  ,
                                                INPUT-OUTPUT iBrowseCol     ,
                                                INPUT-OUTPUT iBrowseRow     , 
                                                INPUT-OUTPUT iBrowseWidth   ,
                                                INPUT-OUTPUT iBrowseHeight) . 
        RUN cdxPositionObjet2 IN  h_dCatalogue ( INPUT        "ECRITURE"     ,                                                 
                                                INPUT-OUTPUT iBrowseCol2     ,
                                                INPUT-OUTPUT iBrowseRow2     , 
                                                INPUT-OUTPUT iBrowseWidth2   ,
                                                INPUT-OUTPUT iBrowseHeight2) . 

        PUBLISH 'cdxAfficheViewer':U.

        ASSIGN hBrowse:SELECTABLE = FALSE
               hBrowse:MOVABLE    = FALSE
               hBrowse:RESIZABLE  = FALSE.
        ASSIGN hBrowse2:SELECTABLE = FALSE
               hBrowse2:MOVABLE    = FALSE
               hBrowse2:RESIZABLE  = FALSE.

        
    END.
    ELSE
        DYNAMIC-FUNCTION("phxMessage",INPUT "Codexia"
                                     ,INPUT 529  /* La position du tableau et le detail du tableau est incorrecte */
                                     ,INPUT ""
                                     ,INPUT "INFORMATION"
                                     ,INPUT "OK"). 

END.
    
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CdxValidDocu1 wWin 
PROCEDURE CdxValidDocu1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER plOk AS LOGICAL     NO-UNDO.
                                         
IF VALID-HANDLE (h_wdocu1) 
   THEN ASSIGN plOk = TRUE .
   ELSE ASSIGN plOk = FALSE .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cdxVertical wWin 
PROCEDURE cdxVertical :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF ttAppliGed.AffDossier = 1 THEN
DO:
    /* viewer */
    PUBLISH "cdxAlignVertical".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY COMBO-BOX-Application Fi-Niveau2 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-27 RECT-28 COMBO-BOX-Application button-DelSsDossier 
         button-AddSsDossier button-UpdSsDossier 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cCode               AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lDroitDesign        AS LOGICAL       NO-UNDO.
DEFINE VARIABLE hWidget             AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hfils               AS HANDLE        NO-UNDO.
DEFINE VARIABLE cChampTri           AS CHARACTER     NO-UNDO.
DEFINE VARIABLE cTri                AS CHARACTER     NO-UNDO.
DEFINE VARIABLE hSdoServer          AS HANDLE        NO-UNDO.

  DYNAMIC-FUNCTION("setopenOninit" IN  h_dCatalogue, INPUT FALSE).  
  DYNAMIC-FUNCTION("setopenOninit" IN  h_dCatalogue2, INPUT FALSE).
  
  RUN SUPER.

  /* Positionnement de la fenetre en haut a gauche */
  DEFINE VARIABLE iLeft   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iTop    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iWidth  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iHeight AS INTEGER     NO-UNDO.

  RUN GetWorkingArea (INPUT {&WINDOW-NAME}:HWND, OUTPUT iLeft, OUTPUT iTop, OUTPUT iWidth, OUTPUT iHeight).
  {&WINDOW-NAME}:X = iLeft.
  {&WINDOW-NAME}:Y = 0.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME} :
      
      SESSION:SET-WAIT-STATE("GENERAL").
      
      /* Recuperer la liste des code AppliGed */
      ASSIGN hSdoServer = DYNAMIC-FUNCTION('getASHandle':U IN h_dCatalogue).
      RUN cdxListeAppliGed IN hSdoServer (OUTPUT clisteAppliGed,
                                          OUTPUT clisteLibAppliGed).
      { lib/ASDisconnect.i h_dCatalogue hSdoServer }
      
      ASSIGN COMBO-BOX-Application:LIST-ITEMS   = clisteLibAppliGed
             COMBO-BOX-Application:SCREEN-VALUE = ENTRY(LOOKUP(cCodeAppliGed,clisteAppliGed),clisteLibAppliGed).

      /* Gestion du premier browse */
      DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dCatalogue,
         INPUT "CodeSociete" /* CHARACTER */,
         INPUT cComSociete /* CHARACTER */,
         INPUT "=" /* CHARACTER */).
    
      DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dCatalogue,
         INPUT "CodeEtab" /* CHARACTER */,
         INPUT cComEtab /* CHARACTER */,
         INPUT "=" /* CHARACTER */).   
    
      DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dCatalogue,
         INPUT "CodeAgence" /* CHARACTER */,
         INPUT cComAgence /* CHARACTER */,
         INPUT "=" /* CHARACTER */).  
    
      DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dCatalogue,
         INPUT "CodeAppliGed" /* CHARACTER */,
         INPUT cCodeAppliGed /* CHARACTER */,
         INPUT "=" /* CHARACTER */).  

      DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dCatalogue,
         INPUT "CodeParent" /* CHARACTER */,
         INPUT "00000" /* CHARACTER */,
         INPUT "=" /* CHARACTER */).  

      DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dCatalogue,
         INPUT "EstActif" /* CHARACTER */,
         INPUT "TRUE" /* CHARACTER */,
         INPUT "=" /* CHARACTER */).  

      /* tri par sdo */
      RUN ref/cdxTriSdo.p (INPUT  cCodeAppliGed ,
                       OUTPUT cChampTri).
      IF cChampTri <> "" AND
         cChampTri <> ?  THEN
      DO:
          ASSIGN cTri = "BY Catalogue." + cChampTri .
          DYNAMIC-FUNCTION('setQuerySort':U  IN h_dCatalogue, INPUT cTri ).            
      END.      
      DYNAMIC-FUNCTION('openQuery':U IN  h_dCatalogue).

      /* Gestion du second Browse */
      DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dCatalogue2,
         INPUT "NoNiveau" /* CHARACTER */,
         INPUT "2" /* CHARACTER */,
         INPUT "=" /* CHARACTER */).  

      DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dCatalogue2,
         INPUT "EstActif" /* CHARACTER */,
         INPUT "TRUE" /* CHARACTER */,
         INPUT "=" /* CHARACTER */).  
      
      /* ... tri defini car non parametrable ...*/
      ASSIGN cTri = "BY Catalogue.NoOrdreAff DESCENDING "  .              

      RUN cdxInitAffichage.

      /* si pas autoriser a modifier de dessign griser tout le menu popup */  
      ASSIGN ldroitDesign = DYNAMIC-FUNCTION("getDroitFonction", "DESIGN"). 
      IF NOT ldroitDesign OR ttAppliGed.AffDossier = 2 /* presentation uniquement du browse */ THEN
      DO:
        ASSIGN hWidget = CURRENT-WINDOW:POPUP-MENU.
            hfils   = hWidget:FIRST-CHILD .
            REPEAT:
                IF NOT VALID-HANDLE(hfils) THEN LEAVE .
                hfils:SENSITIVE = FALSE .
                hfils = hfils:NEXT-SIBLING.
            END.
      END. 
      DYNAMIC-FUNCTION("OpenQuery" IN  h_dCatalogue).

      SESSION:SET-WAIT-STATE("").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadErrorMes wWin 
PROCEDURE ReadErrorMes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE   INPUT PARAMETER phSdo    AS HANDLE NO-UNDO.
    DEFINE  OUTPUT PARAMETER plErreur AS LOG NO-UNDO INITIAL FALSE.    

    EMPTY TEMP-TABLE ttErrorMes .

    RUN getAllErrorMes ( INPUT phSdo, OUTPUT TABLE ttErrorMes).    

    FOR EACH ttErrorMes WHERE ttErrorMes.DejaAffiche = FALSE :
        RUN AffMessage ( INPUT ttErrorMes.CodeFonction ,
                         INPUT ttErrorMes.NoErreur ,
                         INPUT ttErrorMes.Message1 ,
                         INPUT ttErrorMes.NiveauGravite ) .                       
        ASSIGN plErreur = TRUE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RTB_xref_generator wWin 
PROCEDURE RTB_xref_generator :
/* -----------------------------------------------------------
Purpose:    Generate RTB xrefs for SMARTOBJECTS.
Parameters: <none>
Notes:      This code is generated by the UIB.  DO NOT modify it.
            It is included for Roundtable Xref generation. Without
            it, Xrefs for SMARTOBJECTS could not be maintained by
            RTB.  It will in no way affect the operation of this
            program as it never gets executed.
-------------------------------------------------------------*/
  RUN "smt/vCatalogue.w *RTB-SmObj* ".
  RUN "adm2\dyntoolbar.w *RTB-SmObj* ".
  RUN "smt/dCatalogue.w *RTB-SmObj* ".
  RUN "smt/bCatalogue.w *RTB-SmObj* ".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

