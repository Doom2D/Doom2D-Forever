const
  SizeOf_TMapHeaderRec_1 = 452;
  SizeOf_TTextureRec_1 = 65;
  SizeOf_TPanelRec_1 = 18;
  SizeOf_TItemRec_1 = 10;
  SizeOf_TMonsterRec_1 = 10;
  SizeOf_TAreaRec_1 = 10;
  SizeOf_TTriggerRec_1 = 148;

procedure mb_Read_TriggerData (var tr: TTriggerData; ttype: Integer; const buf; bufsize: Integer);
procedure mb_Write_TriggerData (var buf; bufsize: Integer; ttype: Integer; var tr: TTriggerData);
procedure mb_Read_TMapHeaderRec_1 (var tr: TMapHeaderRec_1; const buf; bufsize: Integer);
procedure mb_Write_TMapHeaderRec_1 (var buf; bufsize: Integer; var tr: TMapHeaderRec_1);
procedure mb_Read_TTextureRec_1 (var tr: TTextureRec_1; const buf; bufsize: Integer);
procedure mb_Write_TTextureRec_1 (var buf; bufsize: Integer; var tr: TTextureRec_1);
procedure mb_Read_TPanelRec_1 (var tr: TPanelRec_1; const buf; bufsize: Integer);
procedure mb_Write_TPanelRec_1 (var buf; bufsize: Integer; var tr: TPanelRec_1);
procedure mb_Read_TItemRec_1 (var tr: TItemRec_1; const buf; bufsize: Integer);
procedure mb_Write_TItemRec_1 (var buf; bufsize: Integer; var tr: TItemRec_1);
procedure mb_Read_TMonsterRec_1 (var tr: TMonsterRec_1; const buf; bufsize: Integer);
procedure mb_Write_TMonsterRec_1 (var buf; bufsize: Integer; var tr: TMonsterRec_1);
procedure mb_Read_TAreaRec_1 (var tr: TAreaRec_1; const buf; bufsize: Integer);
procedure mb_Write_TAreaRec_1 (var buf; bufsize: Integer; var tr: TAreaRec_1);
procedure mb_Read_TTriggerRec_1 (var tr: TTriggerRec_1; const buf; bufsize: Integer);
procedure mb_Write_TTriggerRec_1 (var buf; bufsize: Integer; var tr: TTriggerRec_1);
