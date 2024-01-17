
SELECT PRO.[Region_Code]
	  ,PRO.[Region_Name]
	  ,PRO.[STP_Code] as [ICB_Code]
	  ,PRO.[STP_Name] as [ICB_Name]
	  ,RTT.[Treatment_Function_Code]
	  ,TFC.[Treatment_Function_Desc]
	  ,RTT.[Effective_Snapshot_Date]
	  ,RTT.[Report_Period_Length]
      ,SUM([Number_Of_Incomplete_Pathways]) AS [Incomplete_Pathways]
  FROM [NHSE_UKHF].[RTT].[vw_Incomplete_Pathways_Provider1] AS [RTT]

  LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_ODS_Provider_Hierarchies] as PRO
  ON RTT.[Organisation_Code] = PRO.[Organisation_Code] COLLATE Latin1_General_CI_AS

  LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ZZZ_TreatmentFunction] as TFC
  ON RTT.[Treatment_Function_Code] = TFC.[Treatment_Function_Code] COLLATE Latin1_General_CI_AS

  WHERE RTT.[Treatment_Function_Code] != '999'

  GROUP BY PRO.[Region_Code]
	  ,PRO.[Region_Name]
	  ,PRO.[STP_Code]
	  ,PRO.[STP_Name]
	  ,RTT.[Treatment_Function_Code]
	  ,TFC.[Treatment_Function_Desc]
	  ,RTT.[Effective_Snapshot_Date]
	  ,RTT.[Report_Period_Length]

 ORDER BY RTT.[Effective_Snapshot_Date]
		 ,RTT.[Treatment_Function_Code]