

DROP TABLE #AttdInterior;
DROP TABLE #Attd;
DROP TABLE #AttdBySchool;
DROP TABLE #AttdInteriorLast7Days;
DROP TABLE #AttendanceList;
DROP TABLE #NewlyEnrolled;
DROP TABLE #NewlyExited;
DROP TABLE #CurrentlyEnrolled;
DROP TABLE #Enrollment;
DROP TABLE #OnTrackBySchool;
DROP TABLE #OnTrackList;
DROP TABLE #CohortsCnt;
DROP TABLE #MAPgrpCounts;
DROP TABLE #MAPsGrowthList;
DROP TABLE #MAPsProficientList;
DROP TABLE #gradesCounts;
DROP TABLE #GradeDistList;
DROP TABLE #DatesList;
GO




DROP TABLE #AttdInterior;

SELECT 
	schoolType, [SchoolID], groupType, 
	--totalEventCnt, 
	--totalEventCntByschool, 
	COUNT(*) AS totalEventCntByGroup, 
	SUM(IsAbs) AbsCnt, 
	SUM(IsPresent) AS PresentCnt
INTO #AttdInterior
FROM (
	--SELECT 
	--	schoolType,
	--	[SchoolID], StudentEthnicityCode AS groupType, CAST(IsAbs AS INT) AS IsAbs, CAST(~IsAbs AS INT) AS IsPresent
	--	--COUNT(*) OVER ( ) totalEventCnt 
	--FROM	--27832
	--(
	--	SELECT 
	--		mv.EducationOrganizationID AS schoolID,
	--		mv.periodID, mv.studentNumber, mv.StudentEthnicityCode,			
	--		CASE 
	--			WHEN 
	--				CASE ISNULL(mv.TotalRosteredMinutes, 0) 
	--					WHEN 0 THEN NULL 
	--					ELSE CAST(mv.TotalMinutesAttended AS DECIMAL(10, 4)) / CAST(mv.TotalRosteredMinutes AS DECIMAL(10, 4)) 
	--				END < 0.5000 THEN CAST(1 AS BIT) 
	--			ELSE CAST(0 AS BIT) 
	--		END AS IsAbs, 
	--		CASE WHEN s.StudentGradeLevelCode IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN s.StudentGradeLevelCode IN ('06', '07', '08' ) THEN 'MS' WHEN s.StudentGradeLevelCode IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType
	--	FROM [ORION.CIS.CCSD.NET].SDM.Dif.mv_FactAttendanceByDayPeriod_Expanded mv 
	--	INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimStudent s ON mv.studentNumber = s.studentNumber
	--	WHERE s.StudentCurrentRowIndicator = 'Current' 			
	--	--WHERE EducationOrganizationID = '204' --AND [DateKey] = 20210809
	--) L1

	--UNION 


	SELECT 
		schoolType,
		[SchoolID], StudentGradeLevelCode AS groupType, CAST(IsAbs AS INT) AS IsAbs, CAST(~IsAbs AS INT) AS IsPresent
		--COUNT(*) OVER ( PARTITION BY SchoolID ) totalEventCntByschool, 
		--COUNT(*) OVER ( ) totalEventCnt 
	FROM	--27832
	(
		SELECT 
			mv.EducationOrganizationID AS schoolID,
			mv.periodID, mv.studentNumber, CASE s.StudentGradeLevelCode WHEN 'TP' THEN '0K' ELSE s.StudentGradeLevelCode END AS StudentGradeLevelCode,			
			CASE 
				WHEN 
					CASE ISNULL(mv.TotalRosteredMinutes, 0) 
						WHEN 0 THEN NULL 
						ELSE CAST(mv.TotalMinutesAttended AS DECIMAL(10, 4)) / CAST(mv.TotalRosteredMinutes AS DECIMAL(10, 4)) 
					END < 0.5000 THEN CAST(1 AS BIT) 
				ELSE CAST(0 AS BIT) 
			END AS IsAbs, 
			CASE WHEN s.StudentGradeLevelCode IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN s.StudentGradeLevelCode IN ('06', '07', '08' ) THEN 'MS' WHEN s.StudentGradeLevelCode IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType
		FROM [ORION.CIS.CCSD.NET].SDM.Dif.mv_FactAttendanceByDayPeriod_Expanded mv 
		INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimStudent s ON mv.studentNumber = s.studentNumber
		WHERE s.StudentCurrentRowIndicator = 'Current' 		
			AND NOT ( AttendanceEventReasonDescription = 'Office Confirmed Truant' OR AttendanceEventCode = 'Tardy' ) 
		--WHERE EducationOrganizationID = '204' --AND [DateKey] = 20210809
			AND s.StudentGradeLevelCode IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', 'AD', 'UN' )
	) L1
) L2
GROUP BY schoolType, [SchoolID], groupType--, totalEventCnt, totalEventCntByschool
GO



DROP TABLE #Attd;
SELECT *, CASE ISNULL(totalEventCntByGroup, 0) WHEN 0 THEN NULL ELSE CAST(presentCnt AS DECIMAL(15, 4)) / CAST(totalEventCntByGroup AS DECIMAL(15, 4)) END AS PresentPerc
INTO #Attd
FROM #attdInterior
GO



DROP TABLE #AttdBySchool;
SELECT 
	schoolType, 
	schoolID, 
	--[A] AS Asian_AttdPerc, 
	--[B] AS Black_AttdPerc, 
	--[C] AS Cauca_AttdPerc, 
	--[H] AS Hispanics_AttdPerc, 
	--[I] AS NatAmerican_AttdPerc, 
	--[M] AS MultiRacial_AttdPerc, 
	--[P] AS PacIslanders_AttdPerc, 	
	[PK] AS [PK_AttdPerc], 
	[0K] AS [0K_AttdPerc], 
	[01] AS [01_AttdPerc], 
	[02] AS [02_AttdPerc], 
	[03] AS [03_AttdPerc], 
	[04] AS [04_AttdPerc], 
	[05] AS [05_AttdPerc], 
	[06] AS [06_AttdPerc], 
	[07] AS [07_AttdPerc], 
	[08] AS [08_AttdPerc], 
	[09] AS [09_AttdPerc], 
	[10] AS [10_AttdPerc], 
	[11] AS [11_AttdPerc], 
	[12] AS [12_AttdPerc], 
	[AD] AS [AD_AttdPerc], 
	[UN] AS [UN_AttdPerc]
INTO #AttdBySchool
FROM 
( SELECT schoolType, schoolID, groupType, ROUND(PresentPerc, 4, 1) PresentPerc FROM #attd ) mv 
PIVOT ( MAX(PresentPerc) FOR groupType IN ( [A], [B], [C], [H], [I], [M], [P], [0K], [PK], [01], [02], [03], [04], [05], [06], [07], [08], [09], [10], [11], [12], [AD], [UN]  ) ) pv 
ORDER BY schoolType, schoolID 
GO


DECLARE @ActiveYear int = (	SELECT SchoolYear FROM SDM.curr.DimSchoolYear WHERE SchoolYearActiveIndicator = 'Active' );
DECLARE @CurrentQtr VARCHAR(10) = 'Q1';

--DECLARE @DTT DATE = DATEADD(DAY, -1, CAST(GETDATE() AS DATE));
--DECLARE @DTToday DATE = CAST(GETDATE() AS DATE);


DROP TABLE #DatesList;
CREATE TABLE #DatesList ( 
	SchoolYear VARCHAR(10), 
	Term VARCHAR(10), 
	QuarterStartDate DATETIME NULL, 
	QuarterEndDate DATETIME NULL,
	QuarterDatePullDate DATETIME DEFAULT GETDATE()
) 

DECLARE @strSQLDates NVARCHAR(2000) = '
	select q.SchoolYear
		, q.Term
		, MIN(q.startDate) as QuarterStartDate
		, MAX(q.endDate) as QuarterEndDate
		, MAX(q.endDate) + 7 as QuarterDataPullDate
	from (
		select distinct c.endYear as SchoolYear
			, tm.name as Term
			, tm.startDate
			, tm.endDate
		from campus.clark.dbo.Term as tm
			inner join campus.clark.dbo.TermSchedule as ts on ts.termscheduleID = tm.termScheduleID
			inner join campus.clark.dbo.ScheduleStructure as ss on ss.structureID = ts.structureID
			inner join campus.clark.dbo.Calendar as c on c.calendarID = ss.calendarID
		where 1=1
			and c.endYear >= @currYear
			and coalesce(c.summerSchool,0) = 0
			and coalesce(c.exclude,0) = 0
			and tm.name like ''%Q%''
	) as q
	group by q.SchoolYear, q.Term
	order by q.SchoolYear, q.Term
';

INSERT INTO #DatesList
EXEC sp_executesql @strSQLDates, N'@currYear NVARCHAR(10)', @currYear = @ActiveYear


DECLARE @ActiveYear int = (	SELECT SchoolYear FROM SDM.curr.DimSchoolYear WHERE SchoolYearActiveIndicator = 'Active' );
DECLARE @CurrentQtr VARCHAR(10) = 'Q1';

DECLARE @DTYrStart DATE = CAST('2021-08-09' AS DATE);
DECLARE @DTYrEnd DATE = CAST('2021-10-08' AS DATE);
DECLARE @DTQtrStart DATE = CAST('2021-08-09' AS DATE);
DECLARE @DTQtrEnd DATE = CAST('2021-10-08' AS DATE);

SELECT 
	@DTQtrStart = CAST(QuarterStartDate AS DATE), @DTQtrEnd = CAST(QuarterEndDate AS DATE) 
FROM #DatesList WHERE Term = @CurrentQtr;



DROP TABLE #BehaviorQtrIncidentCountsRanked;
SELECT 
	schoolID, schoolType, BehaviorEventDescription, IncidentCount, 
	ROW_NUMBER() OVER ( PARTITION BY schoolID, schoolType ORDER BY IncidentCount DESC, BehaviorEventDescription  ) rnk
INTO #BehaviorQtrIncidentCountsRanked
FROM 
(
	SELECT 
		schoolID, 
		schoolType, 		
		BehaviorEventDescription, 
		COUNT(DISTINCT BehaviorIncidentId) IncidentCount
	FROM (
		SELECT 
			mv.schoolID, mv.studentNumber, 
			CASE WHEN StudentCurrentGrade IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN StudentCurrentGrade IN ('06', '07', '08' ) THEN 'MS' WHEN StudentCurrentGrade IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType, 			
			BehaviorEventDescription, 
			BehaviorIncidentId
		FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
		WHERE 
			schoolYear = @ActiveYear AND 		
			NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps', 'No Behavior Event', 'Habitual Truancy Citation' ) AND
			IncidentDate BETWEEN @DTQtrStart AND @DTQtrEnd
	) L1 
	GROUP BY schoolID, schoolType, BehaviorEventDescription
) L1
ORDER BY schoolID, schoolType, rnk 
;

	

DROP TABLE #BehaviorRemovedStudentsCntQtr;
SELECT 
	schoolID, 
	schoolType, 		
	COUNT(DISTINCT studentNumber) cnt 
INTO #BehaviorRemovedStudentsCntQtr
FROM (
	SELECT 
		mv.schoolID, mv.studentNumber, 
		CASE WHEN StudentCurrentGrade IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN StudentCurrentGrade IN ('06', '07', '08' ) THEN 'MS' WHEN StudentCurrentGrade IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType, 			
		BehaviorIncidentId
	FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
	WHERE 
		schoolYear = @ActiveYear AND 
		--BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) AND
		BehaviorResolutionCode IN ( 'SUS', 'RPC' ) AND 
		IncidentDate BETWEEN @DTQtrStart AND @DTQtrEnd
) L1 
GROUP BY schoolID, schoolType
;



--number of major and number of minor events in the quarter
--distinct count of students who were removed from instruction at least 1 day (RPC and SUS codes)

DROP TABLE #BehaviorTypeQtr;
SELECT schoolID, schoolType, ISNULL([Minor], 0) AS [# of Minor Incident], ISNULL([Major], 0) AS [# of Major Incident]
INTO #BehaviorTypeQtr
FROM 
(
	SELECT 
		schoolID, 
		schoolType, 
		BehaviorResolutionType, 
		COUNT(DISTINCT BehaviorIncidentId) cnt 
	FROM (
		SELECT 
			mv.schoolID, mv.studentNumber, 
			CASE WHEN StudentCurrentGrade IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN StudentCurrentGrade IN ('06', '07', '08' ) THEN 'MS' WHEN StudentCurrentGrade IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType, 
			BehaviorResolutionType, 
			BehaviorIncidentId
		FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
		WHERE 
			schoolYear = @ActiveYear AND 
			NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) AND
			IncidentDate BETWEEN @DTQtrStart AND @DTQtrEnd
	) L1 
	GROUP BY schoolID, schoolType, BehaviorResolutionType
) mv 
PIVOT ( MAX(cnt) FOR BehaviorResolutionType IN ( [Minor], [Major] ) ) pv 
;



DROP TABLE #BehaviorQtr; 
SELECT 
	schoolID, schoolType, ISNULL(COUNT(DISTINCT BehaviorIncidentId), 0) cnt 
INTO #BehaviorQtr
FROM (
	SELECT DISTINCT 
		mv.schoolID, mv.studentNumber, 
		CASE WHEN StudentCurrentGrade IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN StudentCurrentGrade IN ('06', '07', '08' ) THEN 'MS' WHEN StudentCurrentGrade IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType, 
		BehaviorIncidentId 
	FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
	--INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimStudent s ON mv.studentNumber = s.studentNumber
	WHERE 
		schoolYear = @ActiveYear AND 
		NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) AND
		IncidentDate BETWEEN @DTQtrStart AND @DTQtrEnd
) L1
GROUP BY L1.schoolID, L1.schoolID, L1.schoolType
;


DROP TABLE #BehaviorYTD; 
SELECT 
	schoolID, schoolType, ISNULL(COUNT(DISTINCT BehaviorIncidentId), 0) cnt 
INTO #BehaviorYTD
FROM (
	SELECT DISTINCT 
		mv.schoolID, mv.studentNumber, 
		CASE WHEN StudentCurrentGrade IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN StudentCurrentGrade IN ('06', '07', '08' ) THEN 'MS' WHEN StudentCurrentGrade IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType, 
		BehaviorIncidentId 
	FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
	--INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimStudent s ON mv.studentNumber = s.studentNumber
	WHERE 
		schoolYear = @ActiveYear AND 
		NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) AND
		IncidentDate BETWEEN @DTYrStart AND @DTYrEnd
) L1
GROUP BY L1.schoolID, L1.schoolID, L1.schoolType; 

SELECT COUNT(*), COUNT(DISTINCT BehaviorIncidentId), COUNT(DISTINCT StudentNumber) 
FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimEducationOrganization e ON mv.SchoolID = e.EducationOrganizationID
WHERE schoolYear = '2022' AND NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) 
AND e.EducationOrganizationCurrentRowIndicator = 'Current' 
AND BehaviorResolutionType = 'Major'
AND CurrentSchoolDay = '71'
AND PrimaryResolution = '1'
AND BehaviorEventCode = 'USB'


SELECT CurrentSchoolDay, COUNT(*), COUNT(DISTINCT BehaviorIncidentId), COUNT(DISTINCT StudentNumber) 
FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimEducationOrganization e ON mv.SchoolID = e.EducationOrganizationID
WHERE schoolYear = '2022' AND NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) 
AND e.EducationOrganizationCurrentRowIndicator = 'Current'
AND PrimaryResolution = '1'
GROUP BY CurrentSchoolDay 



SELECT * FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
WHERE schoolYear = '2022' AND 
	NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) 
AND PrimaryResolution = '1'

SELECT COUNT(DISTINCT StudentNumber) FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
WHERE schoolYear = '2022' AND 
	NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) 
AND PrimaryResolution = '1'



SELECT DISTINCT 
BehaviorEventCode, BehaviorEventDescription

FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
WHERE schoolYear = '2022' AND 
	NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) 
ORDER BY BehaviorEventDescription


SELECT DISTINCT PrimaryResolution   FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
WHERE schoolYear = '2022' AND NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) 
ORDER BY schoolID

SELECT DISTINCT schoolID  FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
WHERE schoolYear = '2022' AND NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) 

SELECT DISTINCT CurrentSchoolDay FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
WHERE schoolYear = '2022' AND NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) 
ORDER BY schoolID

SELECT * FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
WHERE schoolYear = '2022' AND NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) 
AND PrimaryResolution = 1

SELECT DISTINCT schoolID FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
WHERE schoolYear = '2022' AND NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) 
SELECT DISTINCT StudentEthnicity FROM [ORION.CIS.CCSD.NET].SDM.DIF.mv_BehaviorIncidentsAllResolutions mv 
WHERE schoolYear = '2022' AND NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' ) 


GO
DECLARE @DTYrStart DATE = CAST('2021-08-09' AS DATE);
DECLARE @DTYrEnd DATE = CAST('2021-10-08' AS DATE);
DECLARE @DTQtrStart DATE = CAST('2021-08-09' AS DATE);
DECLARE @DTQtrEnd DATE = CAST('2021-10-08' AS DATE);

SELECT 
	@DTQtrStart = CAST(QuarterStartDate AS DATE), @DTQtrEnd = CAST(QuarterEndDate AS DATE) 
FROM #DatesList WHERE Term = 'Q2';

SELECT 
	@DTYrStart = CAST(QuarterStartDate AS DATE)
FROM #DatesList WHERE Term = 'Q1';
SELECT 
	@DTYrEnd = CAST(QuarterEndDate AS DATE) 
FROM #DatesList WHERE Term = 'Q2';




DROP TABLE #AttendanceList;
SELECT 
	ts.schoolType,
	ts.schoolID, 	
	--ts.[Asian_AttdPerc], ts.[Black_AttdPerc], ts.[Cauca_AttdPerc], ts.[Hispanics_AttdPerc], ts.[NatAmerican_AttdPerc], ts.[MultiRacial_AttdPerc], ts.[PacIslanders_AttdPerc], 
	[PK_AttdPerc], [0K_AttdPerc], [01_AttdPerc], [02_AttdPerc], [03_AttdPerc], [04_AttdPerc], [05_AttdPerc], [06_AttdPerc], [07_AttdPerc], [08_AttdPerc], [09_AttdPerc], [10_AttdPerc], [11_AttdPerc], [12_AttdPerc], [AD_AttdPerc], [UN_AttdPerc]
	
INTO #AttendanceList
--FROM #AttdInteriorLast7Days t7 
FROM #AttdBySchool ts
ORDER BY ts.schoolType, ts.schoolID;
GO


SELECT * FROM SDM.dbo.DimEducationOrganization WHERE CHARINDEX('Durango', EducationOrganizationName) > 0  AND EducationOrganizationCurrentRowIndicator = 'Current'

SELECT * FROM #BehaviorYTD WHERE schoolID = '555' 

DECLARE @ActiveSchoolYear VARCHAR(20) = '2021-2022';
DECLARE @ActiveYear VARCHAR(20) = '2022';

DROP TABLE #FRLschoolsList;
SELECT DISTINCT f.*, RIGHT([School Year], 4) AS schoolYear 
INTO #FRLschoolsList
FROM FRL.[dbo].[FRL Schools] AS f 
WHERE f.[School Year] = @ActiveSchoolYear 
;
DECLARE @DTQtr1Start DATE = CAST('2021-08-09' AS DATE);
DECLARE @DTQtr1End DATE = CAST('2021-10-08' AS DATE);
DECLARE @DT DATE = CAST(GETDATE() AS DATE);
DECLARE @CurrentQtr VARCHAR(10) = 'Q1';

SELECT 
	@DTQtr1Start = CAST(QuarterStartDate AS DATE), @DTQtr1End = CAST(QuarterEndDate AS DATE) 
FROM #DatesList WHERE Term = @CurrentQtr;




DROP TABLE #NewlyEnrolled;
SELECT 
	schoolType, EducationOrganizationID, cntType, COUNT(DISTINCT studentNumber) cnt
INTO #NewlyEnrolled
FROM (
	SELECT 
		CASE WHEN GradeLevelCode IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN GradeLevelCode IN ('06', '07', '08' ) THEN 'MS' WHEN GradeLevelCode IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType,
		EducationOrganizationID, 'NewlyEnrolled' AS cntType, studentNumber, StudentEthnicityCode AS groupType
	FROM [ORION.CIS.CCSD.NET].SDM.Dif.mv_Enrollment 
	WHERE 
		SchoolYearActiveIndicator = 'Active' 
		AND CurrentEnrollment = 1
		AND CAST(EntryDateFullDate AS DATE) BETWEEN @DTQtr1Start AND @DTQtr1End	
) L1
GROUP BY SchoolType, EducationOrganizationID, cntType;


DROP TABLE #NewlyExited;
SELECT 
	schoolType, EducationOrganizationID, cntType, COUNT(DISTINCT studentNumber) cnt
INTO #NewlyExited
FROM (
	SELECT 
		CASE WHEN GradeLevelCode IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN GradeLevelCode IN ('06', '07', '08' ) THEN 'MS' WHEN GradeLevelCode IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType,
		EducationOrganizationID, 
		'NewlyExited' AS cntType, 
		studentNumber	
	FROM [ORION.CIS.CCSD.NET].SDM.Dif.mv_Enrollment 
	WHERE 
		SchoolYearActiveIndicator = 'Active' 
		AND NOT ExitCode = 'No Exit Code'
		AND ExitDateFullDate BETWEEN @DTQtr1Start AND @DTQtr1End
) L1 
GROUP BY SchoolType, EducationOrganizationID, cntType;


DROP TABLE #CurrentlyEnrolled;
SELECT 
	schoolType, EducationOrganizationID, cntType, COUNT(DISTINCT StudentNumber) EnrolledCnt
INTO #CurrentlyEnrolled
FROM (
	SELECT 
		CASE WHEN GradeLevelCode IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN GradeLevelCode IN ('06', '07', '08' ) THEN 'MS' WHEN GradeLevelCode IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType,
		EducationOrganizationID, StudentEthnicityCode AS cntType, 
		StudentNumber	
	FROM [ORION.CIS.CCSD.NET].SDM.Dif.mv_Enrollment 
	WHERE 
		SchoolYear = @ActiveYear 
		AND SchoolYearActiveIndicator = 'Active' 
		AND CurrentEnrollment = 1	
	UNION 
	SELECT 
		CASE WHEN GradeLevelCode IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN GradeLevelCode IN ('06', '07', '08' ) THEN 'MS' WHEN GradeLevelCode IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType,
		EducationOrganizationID, 'IEP' AS cntType, studentNumber
	FROM [ORION.CIS.CCSD.NET].SDM.Dif.mv_Enrollment 
	WHERE 
		SchoolYear = @ActiveYear 
		AND SchoolYearActiveIndicator = 'Active' 
		AND CurrentEnrollment = 1
		AND IEPIndicator = 'IEP'
	UNION 
	SELECT 
		CASE WHEN GradeLevelCode IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN GradeLevelCode IN ('06', '07', '08' ) THEN 'MS' WHEN GradeLevelCode IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType,
		EducationOrganizationID, 'LEP' AS cntType, studentNumber
	FROM [ORION.CIS.CCSD.NET].SDM.Dif.mv_Enrollment
	WHERE 
		SchoolYear = @ActiveYear 
		AND SchoolYearActiveIndicator = 'Active' 
		AND CurrentEnrollment = 1
		AND LEPIndicator = 'LEP'	
	UNION 
	SELECT 
		schoolType,
		EducationOrganizationID, 'FRL' AS cntType, studentNumber 
	FROM ( 
		SELECT 
			CASE WHEN GradeLevelCode IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN GradeLevelCode IN ('06', '07', '08' ) THEN 'MS' WHEN GradeLevelCode IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType,
			EducationOrganizationID, e.studentNumber, 
			IIF(fs.[Local School Number] IS NULL, 0, 1) + IIF(f.[StudentNumber] IS NULL, 0, 1) AS IsFRL
		FROM [ORION.CIS.CCSD.NET].SDM.Dif.mv_Enrollment AS e
		LEFT JOIN [FRL].dbo.[FRL] f ON e.[StudentNumber] = f.StudentNumber		
			AND f.CurrentRowIndicator = 'Current'
			AND f.RowEffectiveDate <= @DT
			AND f.Eligibility IN ( 'F', 'R' )
		LEFT JOIN #FRLschoolsList fs ON e.EducationOrganizationID = fs.[Local School Number] 		
		WHERE 
			e.SchoolYear = @ActiveYear 
			AND e.SchoolYearActiveIndicator = 'Active' 
			AND e.CurrentEnrollment = 1
	) L1 
	WHERE IsFRL > 0 		
) L1 
GROUP BY schoolType, EducationOrganizationID, cntType
GO

DROP TABLE #CombinedEnrolls;
SELECT * 
INTO #CombinedEnrolls
FROM
(
	SELECT * FROM #CurrentlyEnrolled 
	UNION
	SELECT * FROM #NewlyEnrolled
	UNION 
	SELECT * FROM #NewlyExited
) mv;




DROP TABLE #Enrollment;
SELECT 
	schoolType, EducationOrganizationID, 
	ISNULL([A], 0) AS [A], ISNULL([B], 0) AS [B], ISNULL([C], 0) AS [C], ISNULL([H], 0) AS [H], ISNULL([I], 0) AS [I], ISNULL([M], 0) AS [M], ISNULL([P], 0) AS [P], 
	ISNULL([IEP], 0) AS [IEP], ISNULL([LEP], 0) AS [EL], ISNULL([FRL], 0) AS [FRL], 
	ISNULL([NewlyEnrolled], 0) AS [NewlyEnrolled], ISNULL([NewlyExited], 0) AS [NewlyExited], 
	ISNULL([A], 0) + ISNULL([B], 0) + ISNULL([C], 0) + ISNULL([H], 0) + ISNULL([I], 0) + ISNULL([M], 0) + ISNULL([P], 0) AS totalCnt
INTO #Enrollment
FROM #CombinedEnrolls mv
PIVOT ( MAX(EnrolledCnt) FOR cntType IN ( [A], [B], [C], [H], [I], [M], [P], [IEP], [LEP], [FRL], [NewlyEnrolled], [NewlyExited] ) ) pv 
ORDER BY EducationOrganizationID, schoolType;





DECLARE @ActiveSchoolYear VARCHAR(20) = '2021-2022';
DECLARE @ActiveYear VARCHAR(20) = '2022';
DROP TABLE #MAPgrpPercs;
DROP TABLE #MAPsGrowthList;
DROP TABLE #MAPsProficientList;
DROP TABLE #MAPsPercentileList;
DROP TABLE #MAPgrpProjectedProficientPerc;

SELECT 
	schoolType, schoolID, schoolName,
	CONCAT(SeasonID, ' ', Discipline) AS CntType,
	MetProjGrowthPerc, 
	IsProficientPerc
INTO #MAPgrpPercs
FROM (
	SELECT 
		--schoolType, TestedSchoolID AS schoolID, CASE SeasonID WHEN 1 THEN 'Fall' WHEN 2 THEN 'Winter' WHEN 3 THEN 'Spring' END AS SeasonID, Discipline, Cnt, stdCnt, MetprojGrowth, 
		schoolType, schoolID, schoolName,
		CASE SeasonID WHEN 1 THEN 'Fall' WHEN 2 THEN 'Winter' WHEN 3 THEN 'Spring' END AS SeasonID, Discipline, Cnt, stdCnt, MetprojGrowth, 
		CASE WHEN ISNULL(stdCnt, 0) = 0.0000 THEN NULL ELSE ROUND(CAST(MetprojGrowth AS DECIMAL(10, 4)) / CAST(StdCnt AS DECIMAL(10, 4)), 4, 1) END MetProjGrowthPerc,
		CASE WHEN ISNULL(stdCnt, 0) = 0.0000 THEN NULL ELSE ROUND(CAST(IsProficient AS DECIMAL(10, 4)) / CAST(StdCnt AS DECIMAL(10, 4)), 4, 1) END IsProficientPerc
	FROM (
		SELECT 
			schoolType, 
			schoolID, 
			schoolName, 
			SeasonID, Discipline, 
			COUNT(*) Cnt, COUNT(DISTINCT testUID) stdCnt, 
			SUM(MetProjGrowth) MetprojGrowth, 
			SUM(IsProficient) IsProficient			
		FROM (
			SELECT DISTINCT 
				schoolType, 
				--TestedSchoolID, 
				AttendingSchoolID AS schoolID, 
				AttendingSchoolName AS schoolName,
				--TestedSchoolID AS schoolID, 
				--TestedSchoolName AS schoolName, 
				SeasonID, Discipline, testUID,
				ISNULL(MetProjGrowth, 0) AS MetprojGrowth,
				ISNULL(IsProficient, 0) AS IsProficient			
				--COUNT(*) Cnt, COUNT(DISTINCT studentNumber) stdCnt, 
				--SUM(ISNULL(MetProjGrowth, 0)) MetprojGrowth, 
				--SUM(ISNULL(IsProficient, 0)) IsProficient			
			FROM (
				SELECT *, CASE WHEN TestedGrade IN ('K', '1', '2', '3', '4', '5' ) THEN 'ES' WHEN TestedGrade IN ('6', '7', '8' ) THEN 'MS' WHEN TestedGrade IN ( '9', '10', '11', '12' ) THEN 'HS' END AS schoolType
				--FROM [ORION.CIS.CCSD.NET].SDM.bi.mv_MAP_Historical t
				FROM [ORION.CIS.CCSD.NET].sdm.bi.mv_MAP_Current t
				WHERE schoolYear = @ActiveYear --AND SeasonID = '3'
				AND Discipline IN ( 'Reading', 'Mathematics', 'Science' ) 
				AND MetProjGrowth IS NOT NULL
			) L1
		) L2
		GROUP BY 
			schoolType, 
			--TestedSchoolID, 
			--TestedSchoolName,
			SchoolID,
			SchoolName,
			SeasonID, Discipline
	) L1	
) L2
ORDER BY schoolID;


DROP TABLE #MAPgrpProjectedProficientPerc;
SELECT 
	schoolType, schoolID, schoolName,
	CONCAT(SeasonID, ' ', Discipline) AS CntType,
	stdCnt AS Cnt, stdCnt,
	ProjectedProficientCnt,
	ProjectedProficientPerc
INTO #MAPgrpProjectedProficientPerc
FROM (
	SELECT 
		--schoolType, TestedSchoolID AS schoolID, CASE SeasonID WHEN 1 THEN 'Fall' WHEN 2 THEN 'Winter' WHEN 3 THEN 'Spring' END AS SeasonID, Discipline, Cnt, stdCnt, MetprojGrowth, 
		schoolType, schoolID, schoolName,
		CASE SeasonID WHEN 1 THEN 'Fall' WHEN 2 THEN 'Winter' WHEN 3 THEN 'Spring' END AS SeasonID, Discipline, stdCnt, ProjectedProficientCnt,
		CASE WHEN ISNULL(stdCnt, 0) = 0.0000 THEN NULL ELSE ROUND(CAST(ProjectedProficientCnt AS DECIMAL(10, 4)) / CAST(StdCnt AS DECIMAL(10, 4)), 4, 1) END ProjectedProficientPerc		
	FROM (
		SELECT 
			L1.schoolType, 
			--TestedSchoolID, 
			L1.AttendingSchoolID AS schoolID, 
			L1.AttendingSchoolName AS schoolName,
			L1.SeasonID, L1.Discipline, 
			--COUNT(*) Cnt, COUNT(DISTINCT L1.studentNumber) stdCnt, 
			COUNT(DISTINCT L1.testUID) stdCnt, 
			SUM(L1.ProjectedProfStudentID) AS ProjectedProficientCnt
			--SUM( CASE ISNULL(ProjectedCRTLevel, '') WHEN 'Level 3' THEN 1 WHEN 'Level 4' THEN 1 ELSE 0 END ) ProjectedProficientCnt
		--SELECT DISTINCT 
		--	L1.schoolType, 
		--	--TestedSchoolID, 
		--	--L1.TestedSchoolID AS schoolID, 
		--	--L1.TestedSchoolName AS schoolName,
		--	L1.AttendingSchoolID AS schoolID, 
		--	L1.AttendingSchoolName AS schoolName,
		--	L1.SeasonID, L1.Discipline, 
		--	testID, 

		FROM 
		(
			SELECT DISTINCT 
				t.AttendingSchoolID, t.AttendingSchoolName, 
				t.SeasonID, t.Discipline,
				--t.studentNumber, 
				t.testUID, 			
				CASE WHEN NOT ISNULL(ProjectedCRTLevel, '') = '' AND ISNULL(ProjectedCRTLevel, '') IN (  'Level 3', 'Level 4' ) AND TestedGrade IN ('K', '1', '2', '3', '4', '5', '6', '7', '8' ) THEN 1 ELSE 0 END AS ProjectedProfStudentID,			
				CASE WHEN TestedGrade IN ('K', '1', '2', '3', '4', '5' ) THEN 'ES' WHEN TestedGrade IN ('6', '7', '8' ) THEN 'MS' WHEN TestedGrade IN ( '9', '10', '11', '12' ) THEN 'HS' END AS schoolType
			--FROM [ORION.CIS.CCSD.NET].SDM.bi.mv_MAP_Historical t
			FROM [ORION.CIS.CCSD.NET].sdm.bi.mv_MAP_Current t
			WHERE schoolYear = @ActiveYear --AND SeasonID = '3'
			AND Discipline IN ( 'Reading', 'Mathematics', 'Science' ) 
			AND NOT ISNULL(ProjectedCRTLevel, '') = ''
			AND TestedGrade IN ('K', '1', '2', '3', '4', '5', '6', '7', '8' ) 
			UNION 
			SELECT DISTINCT 
				t.AttendingSchoolID, t.AttendingSchoolName, 
				t.SeasonID, t.Discipline,
				--t.studentNumber, 
				t.testUID, 		
				CASE WHEN NOT ISNULL(t.ProjectedACTProficiency, '') = '' AND ISNULL(ProjectedACTProficiency, '') IN ( 'On Track 22', 'On Track 24' ) AND TestedGrade IN ( '9', '10', '11', '12' ) THEN 1 ELSE 0 END AS ProjectedProfStudentID,				
				CASE WHEN TestedGrade IN ('K', '1', '2', '3', '4', '5' ) THEN 'ES' WHEN TestedGrade IN ('6', '7', '8' ) THEN 'MS' WHEN TestedGrade IN ( '9', '10', '11', '12' ) THEN 'HS' END AS schoolType
			--FROM [ORION.CIS.CCSD.NET].SDM.bi.mv_MAP_Historical t
			FROM [ORION.CIS.CCSD.NET].sdm.bi.mv_MAP_Current t
			WHERE schoolYear = @ActiveYear --AND SeasonID = '3'
			AND Discipline IN ( 'Reading', 'Mathematics', 'Science' ) 
			AND NOT ISNULL(t.ProjectedACTProficiency, '') = ''			
			AND TestedGrade IN ( '9', '10', '11', '12' ) 
		) L1 /*
		INNER JOIN ( 
			SELECT 	DISTINCT 
				CASE WHEN TestedGrade IN ('K', '1', '2', '3', '4', '5' ) THEN 'ES' WHEN TestedGrade IN ('6', '7', '8' ) THEN 'MS' WHEN TestedGrade IN ( '9', '10', '11', '12' ) THEN 'HS' END AS schoolType, 
				--TestedSchoolID, 
				AttendingSchoolID,
				SeasonID, Discipline,
				testUID AS ProjectedProfStudentID
				--studentNumber AS ProjectedProfStudentID
			--FROM [ORION.CIS.CCSD.NET].SDM.bi.mv_MAP_Historical t
			FROM [ORION.CIS.CCSD.NET].sdm.bi.mv_MAP_Current t
			WHERE schoolYear = @ActiveYear --AND SeasonID = '3'
			AND Discipline IN ( 'Reading', 'Mathematics', 'Science' ) 
			AND NOT ISNULL(ProjectedCRTLevel, '') = ''
			AND ISNULL(ProjectedCRTLevel, '') IN (  'Level 3', 'Level 4' ) 
			AND TestedGrade IN ('K', '1', '2', '3', '4', '5', '6', '7', '8' ) 
			UNION 
			SELECT 	DISTINCT 
				CASE WHEN TestedGrade IN ('K', '1', '2', '3', '4', '5' ) THEN 'ES' WHEN TestedGrade IN ('6', '7', '8' ) THEN 'MS' WHEN TestedGrade IN ( '9', '10', '11', '12' ) THEN 'HS' END AS schoolType, 
				--TestedSchoolID, 
				AttendingSchoolID,
				SeasonID, Discipline,
				testUID AS ProjectedProfStudentID
				--studentNumber AS ProjectedProfStudentID
			--FROM [ORION.CIS.CCSD.NET].SDM.bi.mv_MAP_Historical t
			FROM [ORION.CIS.CCSD.NET].sdm.bi.mv_MAP_Current t
			WHERE schoolYear = @ActiveYear --AND SeasonID = '3'
			AND Discipline IN ( 'Reading', 'Mathematics', 'Science' ) 
			AND NOT ISNULL(t.ProjectedACTProficiency, '') = ''
			AND ISNULL(ProjectedACTProficiency, '') IN ( 'On Track 22', 'On Track 24' ) 
			AND TestedGrade IN ( '9', '10', '11', '12' ) 
		) L2 
			--ON L1.schoolType = L2.schoolType AND L1.TestedSchoolID = L2.TestedSchoolID AND L1.SeasonID = L2.SeasonID AND L1.Discipline = L2.Discipline
			ON L1.schoolType = L2.schoolType AND L1.AttendingSchoolID = L2.AttendingSchoolID AND L1.SeasonID = L2.SeasonID AND L1.Discipline = L2.Discipline*/
		GROUP BY 
			L1.schoolType, 
			--L1.TestedSchoolID, 
			--L1.TestedSchoolName,
			L1.AttendingSchoolID,
			L1.AttendingSchoolName,
			L1.SeasonID, L1.Discipline
	) L1A
) L2A
ORDER BY schoolID;


--SELECT * FROM #MAPgrpProjectedProficientPerc WHERE schoolID = 374


DECLARE @ActiveYear VARCHAR(20) = '2022';
DECLARE @FirstQuantileLevelCutOff INT = 40;
DECLARE @SecondQuantileLevelCutOff INT = 69;

DROP TABLE #MAPsPercentileList;
SELECT 
	schoolType, 
	schoolID, SchoolName,
	CONCAT(SeasonID, ' ', Discipline) AS CntType,
	stdCnt As Cnt, stdCnt, 
	FortyPercCount, 
	FirstQuantileCount, SecondQuantileCount, ThirdQuantileCount, 
	FortyPercentilePerc,
	FirstQuantilePerc, 
	SecondQuantilePerc, 
	ThirdQuantilePerc
INTO #MAPsPercentileList
FROM (
	SELECT 
		schoolType, 
		schoolName, 
		schoolID, 
		CASE ISNULL(seasonID, '0') WHEN '1' THEN 'Fall' WHEN '2' THEN 'Winter' WHEN '3' THEN 'Spring' END SeasonID,
		Discipline, 
		stdCnt, 
		FortyPercCount,
		FirstQuantileCount,
		SecondQuantileCount,
		ThirdQuantileCount, 
		CASE WHEN ISNULL(stdCnt, 0) = 0.0000 THEN NULL ELSE ROUND(CAST(FortyPercCount AS DECIMAL(10, 4)) / CAST(StdCnt AS DECIMAL(10, 4)), 4, 1) END FortyPercentilePerc,
		CASE WHEN ISNULL(stdCnt, 0) = 0.0000 THEN NULL ELSE ROUND(CAST(FirstQuantileCount AS DECIMAL(10, 4)) / CAST(StdCnt AS DECIMAL(10, 4)), 4, 1) END FirstQuantilePerc,
		CASE WHEN ISNULL(stdCnt, 0) = 0.0000 THEN NULL ELSE ROUND(CAST(SecondQuantileCount AS DECIMAL(10, 4)) / CAST(StdCnt AS DECIMAL(10, 4)), 4, 1) END SecondQuantilePerc,
		CASE WHEN ISNULL(stdCnt, 0) = 0.0000 THEN NULL ELSE ROUND(CAST(ThirdQuantileCount AS DECIMAL(10, 4)) / CAST(StdCnt AS DECIMAL(10, 4)), 4, 1) END ThirdQuantilePerc	
	FROM (
		SELECT 
			schoolType, 
			schoolName, 
			schoolID, 
			seasonID, 
			Discipline, 
			COUNT(DISTINCT testUID) stdCnt, 
			SUM(FortyPercCount) FortyPercCount, 
			SUM(FirstQuantileCount) FirstQuantileCount, 
			SUM(SecondQuantileCount) SecondQuantileCount, 
			SUM(ThirdQuantileCount) ThirdQuantileCount		
		FROM (
			SELECT DISTINCT 
				schoolType, 
				AttendingSchoolName AS schoolName, 
				AttendingSchoolID AS schoolID, 
				--TestedSchoolName AS schoolName,
				--TestedSchoolID AS schoolID,
				SeasonID, Discipline, 
				--studentNumber,
				TestUID, 
				CASE WHEN ISNULL(TestPercentile, 0) >= 40 THEN 1 ELSE 0 END FortyPercCount,
				CASE WHEN ISNULL(TestPercentile, 0) < @FirstQuantileLevelCutOff THEN 1 ELSE 0 END FirstQuantileCount,
				CASE WHEN ISNULL(TestPercentile, 0) >= @FirstQuantileLevelCutOff  AND ISNULL(TestPercentile, 0) <= @SecondQuantileLevelCutOff THEN 1 ELSE 0 END SecondQuantileCount, 
				CASE WHEN ISNULL(TestPercentile, 0) > @SecondQuantileLevelCutOff THEN 1 ELSE 0 END ThirdQuantileCount
			FROM (
				SELECT 
					*, CASE WHEN TestedGrade IN ('K', '1', '2', '3', '4', '5' ) THEN 'ES' WHEN TestedGrade IN ('6', '7', '8' ) THEN 'MS' WHEN TestedGrade IN ( '9', '10', '11', '12' ) THEN 'HS' END AS schoolType
				FROM [ORION.CIS.CCSD.NET].sdm.bi.mv_MAP_Current t
				WHERE schoolYear = @ActiveYear --AND SeasonID = '3'
				AND Discipline IN ( 'Reading', 'Mathematics', 'Science' ) 			
			) L1
		) L2
		GROUP BY 
			schoolType, 
			SchoolID, 
			SchoolName,
			--TestedSchoolName,
			--TestedSchoolID,
			SeasonID, Discipline
	) L3	
) L4
ORDER BY schoolID;


--DROP TABLE #MAPgrpDenomCounts;
--SELECT 
--	schoolType, schoolID, 
--	CONCAT(SeasonID, ' ', Discipline, ' Partic Std #') AS CntType,
--	StdCnt
--INTO #MAPgrpDenomCounts
--FROM (
--	SELECT 
--		schoolType, schoolID, CASE SeasonID WHEN 1 THEN 'Fall' WHEN 2 THEN 'Winter' WHEN 3 THEN 'Spring' END AS SeasonID, Discipline,  MetprojGrowth, 
--		ISNULL(stdCnt, 0) StdCnt, 
--		ISNULL(Cnt, 0) Cnt
--	FROM (
--		SELECT 
--			schoolType, schoolID, SeasonID, Discipline, COUNT(*) Cnt, 
--			COUNT(DISTINCT studentNumber) stdCnt, 
--			--COUNT(studentNumber) stdCnt, 
--			SUM(ISNULL(MetProjGrowth, 0)) MetprojGrowth, SUM(ISNULL(IsProficient, 0)) IsProficient
--		FROM (
--			--SELECT *, CASE WHEN TestedGrade IN ('K', '1', '2', '3', '4', '5' ) THEN 'ES' WHEN TestedGrade IN ('6', '7', '8' ) THEN 'MS' WHEN TestedGrade IN ( '9', '10', '11', '12' ) THEN 'HS' END AS schoolType
--			SELECT 
--				TestedSchoolID AS schoolID, 
--				SeasonID, Discipline, studentNumber, MetProjGrowth, IsProficient,
--				CASE WHEN TestedGrade IN ('K', '1', '2', '3', '4', '5' ) THEN 'ES' WHEN TestedGrade IN ('6', '7', '8' ) THEN 'MS' WHEN TestedGrade IN ( '9', '10', '11', '12' ) THEN 'HS' END AS schoolType
--			FROM [ORION.CIS.CCSD.NET].SDM.bi.mv_MAP_Historical t
--			WHERE schoolYear = @ActiveYear --AND SeasonID = '3'
--			AND Discipline IN ( 'Reading', 'Mathematics', 'Science' ) 
--			AND MetProjGrowth IS NOT NULL
--		) L1
--		GROUP BY 
--			schoolType, SchoolID, SeasonID, Discipline
--	) L1
--) L2 
--;


--SELECT * FROM [ORION.CIS.CCSD.NET].SDM.bi.mv_MAP_Current t
--WHERE schoolYear = '2022' AND SeasonID = 1
--	AND Discipline ='Mathematics' 
--	AND TestedSchoolID = '412'
--	AND MetProjGrowth IS NULL
--			--AND Discipline IN ( 'Reading', 'Mathematics', 'Science' ) 
--			--AND MetProjGrowth IS NOT NULL

SELECT * FROM #MAPgrpPercs
SELECT * FROM #MAPgrpProjectedProficientPerc
SELECT * FROM #MAPsPercentileList

DROP TABLE #MAPsList;
SELECT 
	schoolType, schoolID, 
	[Fall Mathematics Growth Perc], [Fall Reading Growth Perc], [Fall Science Growth Perc], 
	[Winter Mathematics Growth Perc], [Winter Reading Growth Perc], [Winter Science Growth Perc], 
	[Spring Mathematics Growth Perc], [Spring Reading Growth Perc], [Spring Science Growth Perc],
	[Fall Mathematics IsProficient Perc], [Fall Reading IsProficient Perc], [Fall Science IsProficient Perc], 
	[Winter Mathematics IsProficient Perc], [Winter Reading IsProficient Perc], [Winter Science IsProficient Perc], 
	[Spring Mathematics IsProficient Perc], [Spring Reading IsProficient Perc], [Spring Science IsProficient Perc], 
	[Fall Mathematics Partic Std #], [Fall Reading Partic Std #], [Fall Science Partic Std #], 
	[Winter Mathematics Partic Std #], [Winter Reading Partic Std #], [Winter Science Partic Std #], 
	[Spring Mathematics Partic Std #], [Spring Reading Partic Std #], [Spring Science Partic Std #], 
	[Fall Mathematics To40Perc], [Fall Mathematics 41To69Perc], [Fall Mathematics Above70Perc], [Fall Mathematics To40Cnt], [Fall Mathematics 41To69Cnt], [Fall Mathematics Above70Cnt], [Fall Mathematics ProjectedProfPerc], [Fall Mathematics ProjectedProfCnt], [Fall Mathematics ProjectedProfPopulationCnt], 
	[Fall Reading To40Perc], [Fall Reading 41To69Perc], [Fall Reading Above70Perc], [Fall Reading To40Cnt], [Fall Reading 41To69Cnt], [Fall Reading Above70Cnt], [Fall Reading ProjectedProfPerc], [Fall Reading ProjectedProfCnt], [Fall Reading ProjectedProfPopulationCnt],
	[Fall Science To40Perc], [Fall Science 41To69Perc], [Fall Science Above70Perc], [Fall Science To40Cnt], [Fall Science 41To69Cnt], [Fall Science Above70Cnt], [Fall Science ProjectedProfPerc], [Fall Science ProjectedProfCnt], [Fall Science ProjectedProfPopulationCnt],	
	[Winter Mathematics To40Perc], [Winter Mathematics 41To69Perc], [Winter Mathematics Above70Perc], [Winter Mathematics To40Cnt], [Winter Mathematics 41To69Cnt], [Winter Mathematics Above70Cnt], [Winter Mathematics ProjectedProfPerc], [Winter Mathematics ProjectedProfCnt], [Winter Mathematics ProjectedProfPopulationCnt], 
	[Winter Reading To40Perc], [Winter Reading 41To69Perc], [Winter Reading Above70Perc], [Winter Reading To40Cnt], [Winter Reading 41To69Cnt], [Winter Reading Above70Cnt], [Winter Reading ProjectedProfPerc], [Winter Reading ProjectedProfCnt], [Winter Reading ProjectedProfPopulationCnt], 
	[Winter Science To40Perc], [Winter Science 41To69Perc], [Winter Science Above70Perc], [Winter Science To40Cnt], [Winter Science 41To69Cnt], [Winter Science Above70Cnt], [Winter Science ProjectedProfPerc], [Winter Science ProjectedProfCnt], [Winter Science ProjectedProfPopulationCnt], 
	[Spring Mathematics To40Perc], [Spring Mathematics 41To69Perc], [Spring Mathematics Above70Perc], [Spring Mathematics To40Cnt], [Spring Mathematics 41To69Cnt], [Spring Mathematics Above70Cnt], [Spring Mathematics ProjectedProfPerc], [Spring Mathematics ProjectedProfCnt], [Spring Mathematics ProjectedProfPopulationCnt], 
	[Spring Reading To40Perc], [Spring Reading 41To69Perc], [Spring Reading Above70Perc], [Spring Reading To40Cnt], [Spring Reading 41To69Cnt], [Spring Reading Above70Cnt], [Spring Reading ProjectedProfPerc], [Spring Reading ProjectedProfCnt], [Spring Reading ProjectedProfPopulationCnt], 
	[Spring Science To40Perc], [Spring Science 41To69Perc], [Spring Science Above70Perc], [Spring Science To40Cnt], [Spring Science 41To69Cnt], [Spring Science Above70Cnt], [Spring Science ProjectedProfPerc], [Spring Science ProjectedProfCnt], [Spring Science ProjectedProfPopulationCnt]
INTO #MAPsList
FROM 
( 
	SELECT schoolType, schoolID, CONCAT(CntType, ' Growth Perc') AS CntType, MetProjGrowthPerc AS Cnt FROM #MAPgrpPercs
	UNION
	SELECT schoolType, schoolID, CONCAT(CntType, ' IsProficient Perc') AS CntType, IsProficientPerc AS Cnt FROM #MAPgrpPercs
	--UNION 
	--SELECT schoolType, schoolID, CntType, StdCnt AS Cnt FROM #MAPgrpDenomCounts
	UNION 
	SELECT schoolType, schoolID, CONCAT(CntType, ' To40Perc') AS CntType, FirstQuantilePerc AS Cnt FROM #MAPsPercentileList
	UNION 
	SELECT schoolType, schoolID, CONCAT(CntType, ' To40Cnt') AS CntType, FirstQuantileCount AS Cnt FROM #MAPsPercentileList
	UNION 
	SELECT schoolType, schoolID, CONCAT(CntType, ' 41To69Perc') AS CntType, SecondQuantilePerc AS Cnt FROM #MAPsPercentileList
	UNION 
	SELECT schoolType, schoolID, CONCAT(CntType, ' 41To69Cnt') AS CntType, SecondQuantileCount AS Cnt FROM #MAPsPercentileList
	UNION 
	SELECT schoolType, schoolID, CONCAT(CntType, ' Above70Perc') AS CntType, ThirdQuantilePerc AS Cnt FROM #MAPsPercentileList
	UNION 
	SELECT schoolType, schoolID, CONCAT(CntType, ' Above70Cnt') AS CntType, ThirdQuantileCount AS Cnt FROM #MAPsPercentileList
	UNION 
	SELECT schoolType, schoolID, CONCAT(CntType, ' ProjectedProfPerc') AS CntType, ProjectedProficientPerc AS Cnt FROM #MAPgrpProjectedProficientPerc
	UNION 
	SELECT schoolType, schoolID, CONCAT(CntType, ' ProjectedProfCnt') AS CntType, ProjectedProficientCnt AS Cnt FROM #MAPgrpProjectedProficientPerc
	UNION 
	SELECT schoolType, schoolID, CONCAT(CntType, ' ProjectedProfPopulationCnt') AS CntType, stdCnt AS Cnt FROM #MAPgrpProjectedProficientPerc
) mv 
PIVOT ( MAX(Cnt) FOR CntType IN ( 
	[Fall Mathematics Growth Perc], [Fall Reading Growth Perc], [Fall Science Growth Perc], 
	[Winter Mathematics Growth Perc], [Winter Reading Growth Perc], [Winter Science Growth Perc], 
	[Spring Mathematics Growth Perc], [Spring Reading Growth Perc], [Spring Science Growth Perc],
	[Fall Mathematics IsProficient Perc], [Fall Reading IsProficient Perc], [Fall Science IsProficient Perc], 
	[Winter Mathematics IsProficient Perc], [Winter Reading IsProficient Perc], [Winter Science IsProficient Perc], 
	[Spring Mathematics IsProficient Perc], [Spring Reading IsProficient Perc], [Spring Science IsProficient Perc], 
	[Fall Mathematics Partic Std #], [Fall Reading Partic Std #], [Fall Science Partic Std #], 
	[Winter Mathematics Partic Std #], [Winter Reading Partic Std #], [Winter Science Partic Std #], 
	[Spring Mathematics Partic Std #], [Spring Reading Partic Std #], [Spring Science Partic Std #], 
	[Fall Mathematics To40Perc], [Fall Mathematics 41To69Perc], [Fall Mathematics Above70Perc], 
	[Fall Mathematics To40Cnt], [Fall Mathematics 41To69Cnt], [Fall Mathematics Above70Cnt], 
	[Fall Reading To40Perc], [Fall Reading 41To69Perc], [Fall Reading Above70Perc], 
	[Fall Reading To40Cnt], [Fall Reading 41To69Cnt], [Fall Reading Above70Cnt], 
	[Fall Science To40Perc], [Fall Science 41To69Perc], [Fall Science Above70Perc], 
	[Fall Science To40Cnt], [Fall Science 41To69Cnt], [Fall Science Above70Cnt], 
	[Winter Mathematics To40Perc], [Winter Mathematics 41To69Perc], [Winter Mathematics Above70Perc], 
	[Winter Mathematics To40Cnt], [Winter Mathematics 41To69Cnt], [Winter Mathematics Above70Cnt], 
	[Winter Reading To40Perc], [Winter Reading 41To69Perc], [Winter Reading Above70Perc], 
	[Winter Reading To40Cnt], [Winter Reading 41To69Cnt], [Winter Reading Above70Cnt], 
	[Winter Science To40Perc], [Winter Science 41To69Perc], [Winter Science Above70Perc], 
	[Winter Science To40Cnt], [Winter Science 41To69Cnt], [Winter Science Above70Cnt], 
	[Spring Mathematics To40Perc], [Spring Mathematics 41To69Perc], [Spring Mathematics Above70Perc], 
	[Spring Mathematics To40Cnt], [Spring Mathematics 41To69Cnt], [Spring Mathematics Above70Cnt], 
	[Spring Reading To40Perc], [Spring Reading 41To69Perc], [Spring Reading Above70Perc], 
	[Spring Reading To40Cnt], [Spring Reading 41To69Cnt], [Spring Reading Above70Cnt], 
	[Spring Science To40Perc], [Spring Science 41To69Perc], [Spring Science Above70Perc],
	[Spring Science To40Cnt], [Spring Science 41To69Cnt], [Spring Science Above70Cnt],
	[Fall Mathematics ProjectedProfPerc], [Fall Mathematics ProjectedProfCnt], [Fall Mathematics ProjectedProfPopulationCnt], 
	[Fall Reading ProjectedProfPerc], [Fall Reading ProjectedProfCnt], [Fall Reading ProjectedProfPopulationCnt],
	[Fall Science ProjectedProfPerc], [Fall Science ProjectedProfCnt], [Fall Science ProjectedProfPopulationCnt],	
	[Winter Mathematics ProjectedProfPerc], [Winter Mathematics ProjectedProfCnt], [Winter Mathematics ProjectedProfPopulationCnt], 
	[Winter Reading ProjectedProfPerc], [Winter Reading ProjectedProfCnt], [Winter Reading ProjectedProfPopulationCnt], 
	[Winter Science ProjectedProfPerc], [Winter Science ProjectedProfCnt], [Winter Science ProjectedProfPopulationCnt], 
	[Spring Mathematics ProjectedProfPerc], [Spring Mathematics ProjectedProfCnt], [Spring Mathematics ProjectedProfPopulationCnt], 
	[Spring Reading ProjectedProfPerc], [Spring Reading ProjectedProfCnt], [Spring Reading ProjectedProfPopulationCnt], 
	[Spring Science ProjectedProfPerc], [Spring Science ProjectedProfCnt], [Spring Science ProjectedProfPopulationCnt]  
) ) pv;
;
GO

SELECT * FROM #MAPsList WHERE schoolId = '545'
SELECT * FROM #MAPsList WHERE schoolId = '374'
SELECT * FROM #MAPsPercentileList WHERE schoolId = '545'

DECLARE @CurrentQtr VARCHAR(10) = 'Q1';
DECLARE @CurrentSemester VARCHAR(10) = 'S1';
DECLARE @ActiveYear int = (	SELECT SchoolYear FROM SDM.curr.DimSchoolYear WHERE SchoolYearActiveIndicator = 'Active' );
DECLARE @ValidationDate int = ( SELECT CAST( CAST(@ActiveYear-1 AS varchar) + '1001' AS int) );


--DECLARE @HS_Grade_Term_Description VARCHAR(100) = '2021-2022, S1, Semester Grade';
DECLARE @HS_Grade_Qtr_Description VARCHAR(100);
DECLARE @MS_Grade_Qtr_Description VARCHAR(100);
DECLARE @ES_Grade_Qtr_Description VARCHAR(100);
DECLARE @ES_Grade_Qtr_Description2 VARCHAR(100);

DECLARE @HS_Grade_Semester_Description VARCHAR(100);
DECLARE @MS_Grade_Semester_Description VARCHAR(100);
DECLARE @ES_Grade_Semester_Description VARCHAR(100);
DECLARE @ES_Grade_Semester_Description2 VARCHAR(100);

DECLARE @DT DATE = CAST(GETDATE() AS DATE);

IF @CurrentQtr = 'Q1' 
BEGIN
	SET @HS_Grade_Qtr_Description = '2021-2022, Q1, Quarter Grade'
	SET @MS_Grade_Qtr_Description = '2021-2022, Q1, Quarter Grade';
	SET @ES_Grade_Qtr_Description = '2021-2022, Y, Progress Grade';
	SET @ES_Grade_Qtr_Description2 = '2021-2022, S1, Progress Grade';

	SET @HS_Grade_Semester_Description  = '2021-2022, Q2, Semester Grade';
	SET @MS_Grade_Semester_Description  = '2021-2022, Q2, Semester Grade';
	SET @ES_Grade_Semester_Description  = '2021-2022, Y, Progress Grade';
	SET @ES_Grade_Semester_Description2 = '2021-2022, S1, Progress Grade';

END
IF @CurrentQtr = 'Q2'
BEGIN
	SET @HS_Grade_Qtr_Description = '2021-2022, Q2, Quarter Grade'
	SET @MS_Grade_Qtr_Description = '2021-2022, Q2, Quarter Grade';
	SET @ES_Grade_Qtr_Description = '2021-2022, Y, Progress Grade';
	SET @ES_Grade_Qtr_Description2 = '2021-2022, S1, Progress Grade';

	SET @HS_Grade_Semester_Description  = '2021-2022, Q2, Semester Grade';
	SET @MS_Grade_Semester_Description  = '2021-2022, Q2, Semester Grade';
	SET @ES_Grade_Semester_Description  = '2021-2022, Y, Progress Grade';
	SET @ES_Grade_Semester_Description2 = '2021-2022, S1, Progress Grade';
END
IF @CurrentQtr = 'Q3'
BEGIN
	SET @HS_Grade_Qtr_Description = '2021-2022, Q3, Quarter Grade'
	SET @MS_Grade_Qtr_Description = '2021-2022, Q3, Quarter Grade';
	SET @ES_Grade_Qtr_Description = '2021-2022, Y, Progress Grade';
	SET @ES_Grade_Qtr_Description2 = '2021-2022, S2, Progress Grade';
END
IF @CurrentQtr = 'Q4'
BEGIN
	SET @HS_Grade_Qtr_Description = '2021-2022, Q4, Quarter Grade'
	SET @MS_Grade_Qtr_Description = '2021-2022, Q4, Quarter Grade';
	SET @ES_Grade_Qtr_Description = '2021-2022, Y, Progress Grade';
	SET @ES_Grade_Qtr_Description2 = '2021-2022, S2, Progress Grade';

	SET @HS_Grade_Semester_Description  = '2021-2022, S2, Progress Grade';
	SET @MS_Grade_Semester_Description  = '2021-2022, S2, Progress Grade';
	SET @ES_Grade_Semester_Description  = '2021-2022, Y, Progress Grade';
	SET @ES_Grade_Semester_Description2 = '2021-2022, S2, Progress Grade';
END


DROP TABLE #gradesCounts;
SELECT *	
INTO #gradesCounts
FROM (
	SELECT 
		'ES' AS SchoolType,
		@CurrentQtr AS term, 
		gd.EducationOrganizationID, 		
		SUM(CASE WHEN gd.courseGradeLetter = 'A' THEN 1 ELSE 0 END) AS letterACnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'B' THEN 1 ELSE 0 END) AS letterBCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'C' THEN 1 ELSE 0 END) AS letterCCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'D' THEN 1 ELSE 0 END) AS letterDCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'F' THEN 1 ELSE 0 END) AS letterFCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'E' THEN 1 ELSE 0 END) AS letterECnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'S' THEN 1 ELSE 0 END) AS letterSCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'N' THEN 1 ELSE 0 END) AS letterNCnt,
		SUM(CASE WHEN gd.courseGradeLetter = '1' THEN 1 ELSE 0 END) AS letter1Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '2' THEN 1 ELSE 0 END) AS letter2Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '3' THEN 1 ELSE 0 END) AS letter3Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '4' THEN 1 ELSE 0 END) AS letter4Cnt
	FROM [ORION.CIS.CCSD.NET].SDM.[BI].[mv_GradeDistribution_ES] AS gd
	INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimCourse cr ON gd.CourseCode = cr.CourseCode 
	WHERE 
		cr.CourseCurrentRowIndicator = 'Current' AND
		gd.schoolYear = @ActiveYear
		AND (	
				gd.SchoolYearTermCourseGradeTypeDescription = @ES_Grade_Qtr_Description OR 
				( gd.SchoolYearTermCourseGradeTypeDescription  = @ES_Grade_Qtr_Description2 AND gd.EducationOrganizationId IN( '386', '627' ) )
			)
		AND cr.NCESSubjectCode IN ( '01', '02', '03', '04', '53', '54' )
	--AND InProgressGradeIndicator = 'In Progress'	
	GROUP BY gd.EducationOrganizationId 
		
	UNION
	SELECT 
		'MS' AS SchoolType,
		@CurrentQtr AS term,
		EducationOrganizationID, 		
		--SchoolYearTermCourseGradeTypeDescription,		
		SUM(CASE WHEN gd.courseGradeLetter = 'A' THEN 1 ELSE 0 END) AS letterACnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'B' THEN 1 ELSE 0 END) AS letterBCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'C' THEN 1 ELSE 0 END) AS letterCCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'D' THEN 1 ELSE 0 END) AS letterDCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'F' THEN 1 ELSE 0 END) AS letterFCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'E' THEN 1 ELSE 0 END) AS letterECnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'S' THEN 1 ELSE 0 END) AS letterSCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'N' THEN 1 ELSE 0 END) AS letterNCnt,
		SUM(CASE WHEN gd.courseGradeLetter = '1' THEN 1 ELSE 0 END) AS letter1Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '2' THEN 1 ELSE 0 END) AS letter2Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '3' THEN 1 ELSE 0 END) AS letter3Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '4' THEN 1 ELSE 0 END) AS letter4Cnt
		--COUNT(DISTINCT studentNumber) stdCnt
	FROM [ORION.CIS.CCSD.NET].SDM.[BI].[mv_GradeDistribution_MS] AS gd
	INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimCourse cr ON gd.CourseCode = cr.CourseCode 
	WHERE 
		cr.CourseCurrentRowIndicator = 'Current' AND
		gd.schoolYear = @ActiveYear --AND courseGradeLetter IN ( 'A', 'B', 'C', 'D', 'F' ) 
		--AND CHARINDEX('Progress Grade', SchoolYearTermCourseGradeTypeDescription)=0
		AND gd.SchoolYearTermCourseGradeTypeDescription IN ( @MS_Grade_Qtr_Description ) --( '2021-2022, S1, Quarter Grade', '2021-2022, Q1, Quarter Grade'  )
		--AND InProgressGradeIndicator = 'In Progress'	
		AND cr.NCESSubjectCode IN ( '01', '02', '03', '04', '53', '54' )
	GROUP BY gd.EducationOrganizationId --, SchoolYearTermCourseGradeTypeDescription
		
	UNION
	SELECT 
		'HS' AS SchoolType,
		@CurrentQtr AS term,		
		EducationOrganizationID, 		
		SUM(CASE WHEN gd.courseGradeLetter = 'A' THEN 1 ELSE 0 END) AS letterACnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'B' THEN 1 ELSE 0 END) AS letterBCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'C' THEN 1 ELSE 0 END) AS letterCCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'D' THEN 1 ELSE 0 END) AS letterDCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'F' THEN 1 ELSE 0 END) AS letterFCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'E' THEN 1 ELSE 0 END) AS letterECnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'S' THEN 1 ELSE 0 END) AS letterSCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'N' THEN 1 ELSE 0 END) AS letterNCnt,
		SUM(CASE WHEN gd.courseGradeLetter = '1' THEN 1 ELSE 0 END) AS letter1Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '2' THEN 1 ELSE 0 END) AS letter2Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '3' THEN 1 ELSE 0 END) AS letter3Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '4' THEN 1 ELSE 0 END) AS letter4Cnt
	FROM [ORION.CIS.CCSD.NET].SDM.[BI].[mv_GradeDistribution_HS] AS gd
	INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimCourse cr ON gd.CourseCode = cr.CourseCode 
	WHERE 
		cr.CourseCurrentRowIndicator = 'Current' AND
		gd.schoolYear = @ActiveYear
		AND CHARINDEX('Progress Grade', gd.SchoolYearTermCourseGradeTypeDescription)=0
		AND gd.SchoolYearTermCourseGradeTypeDescription IN ( @HS_Grade_Qtr_Description ) --( '2021-2022, S1, Quarter Grade', '2021-2022, Q1, Quarter Grade' )				
		AND cr.NCESSubjectCode IN ( '01', '02', '03', '04', '53', '54' )
	GROUP BY gd.EducationOrganizationId, gd.SchoolYearTermCourseGradeTypeDescription


	/* Semester Grades */
	UNION 
	SELECT 
		'ES' AS SchoolType,
		@CurrentSemester AS term, 
		gd.EducationOrganizationID, 		
		SUM(CASE WHEN gd.courseGradeLetter = 'A' THEN 1 ELSE 0 END) AS letterACnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'B' THEN 1 ELSE 0 END) AS letterBCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'C' THEN 1 ELSE 0 END) AS letterCCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'D' THEN 1 ELSE 0 END) AS letterDCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'F' THEN 1 ELSE 0 END) AS letterFCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'E' THEN 1 ELSE 0 END) AS letterECnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'S' THEN 1 ELSE 0 END) AS letterSCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'N' THEN 1 ELSE 0 END) AS letterNCnt,
		SUM(CASE WHEN gd.courseGradeLetter = '1' THEN 1 ELSE 0 END) AS letter1Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '2' THEN 1 ELSE 0 END) AS letter2Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '3' THEN 1 ELSE 0 END) AS letter3Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '4' THEN 1 ELSE 0 END) AS letter4Cnt
	FROM [ORION.CIS.CCSD.NET].SDM.[BI].[mv_GradeDistribution_ES] AS gd
	INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimCourse cr ON gd.CourseCode = cr.CourseCode 
	WHERE 
		cr.CourseCurrentRowIndicator = 'Current' AND
		gd.schoolYear = @ActiveYear
		AND @CurrentQtr IN ( 'Q2', 'Q4', 'Q1') 
		AND (	
				gd.SchoolYearTermCourseGradeTypeDescription = @ES_Grade_Semester_Description OR 
				( gd.SchoolYearTermCourseGradeTypeDescription  = @ES_Grade_Semester_Description2 AND gd.EducationOrganizationId IN( '386', '627' ) )
			)
		AND cr.NCESSubjectCode IN ( '01', '02', '03', '04', '53', '54' )
	--AND InProgressGradeIndicator = 'In Progress'	
	GROUP BY gd.EducationOrganizationId 
		
	UNION
	SELECT 
		'MS' AS SchoolType,
		@CurrentSemester AS term,
		EducationOrganizationID, 		
		--SchoolYearTermCourseGradeTypeDescription,		
		SUM(CASE WHEN gd.courseGradeLetter = 'A' THEN 1 ELSE 0 END) AS letterACnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'B' THEN 1 ELSE 0 END) AS letterBCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'C' THEN 1 ELSE 0 END) AS letterCCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'D' THEN 1 ELSE 0 END) AS letterDCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'F' THEN 1 ELSE 0 END) AS letterFCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'E' THEN 1 ELSE 0 END) AS letterECnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'S' THEN 1 ELSE 0 END) AS letterSCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'N' THEN 1 ELSE 0 END) AS letterNCnt,
		SUM(CASE WHEN gd.courseGradeLetter = '1' THEN 1 ELSE 0 END) AS letter1Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '2' THEN 1 ELSE 0 END) AS letter2Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '3' THEN 1 ELSE 0 END) AS letter3Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '4' THEN 1 ELSE 0 END) AS letter4Cnt
		--COUNT(DISTINCT studentNumber) stdCnt
	FROM [ORION.CIS.CCSD.NET].SDM.[BI].[mv_GradeDistribution_MS] AS gd
	INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimCourse cr ON gd.CourseCode = cr.CourseCode 
	WHERE 
		cr.CourseCurrentRowIndicator = 'Current' AND
		gd.schoolYear = @ActiveYear --AND courseGradeLetter IN ( 'A', 'B', 'C', 'D', 'F' ) 
		AND @CurrentQtr IN ( 'Q2', 'Q4', 'Q1') 
		--AND CHARINDEX('Progress Grade', SchoolYearTermCourseGradeTypeDescription)=0
		AND gd.SchoolYearTermCourseGradeTypeDescription IN ( @MS_Grade_Semester_Description ) --( '2021-2022, S1, Quarter Grade', '2021-2022, Q1, Quarter Grade'  )
		--AND InProgressGradeIndicator = 'In Progress'	
		AND cr.NCESSubjectCode IN ( '01', '02', '03', '04', '53', '54' )
	GROUP BY gd.EducationOrganizationId --, SchoolYearTermCourseGradeTypeDescription
		
	UNION
	SELECT 
		'HS' AS SchoolType,
		@CurrentSemester AS term,		
		EducationOrganizationID, 		
		SUM(CASE WHEN gd.courseGradeLetter = 'A' THEN 1 ELSE 0 END) AS letterACnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'B' THEN 1 ELSE 0 END) AS letterBCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'C' THEN 1 ELSE 0 END) AS letterCCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'D' THEN 1 ELSE 0 END) AS letterDCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'F' THEN 1 ELSE 0 END) AS letterFCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'E' THEN 1 ELSE 0 END) AS letterECnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'S' THEN 1 ELSE 0 END) AS letterSCnt,
		SUM(CASE WHEN gd.courseGradeLetter = 'N' THEN 1 ELSE 0 END) AS letterNCnt,
		SUM(CASE WHEN gd.courseGradeLetter = '1' THEN 1 ELSE 0 END) AS letter1Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '2' THEN 1 ELSE 0 END) AS letter2Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '3' THEN 1 ELSE 0 END) AS letter3Cnt,
		SUM(CASE WHEN gd.courseGradeLetter = '4' THEN 1 ELSE 0 END) AS letter4Cnt
	FROM [ORION.CIS.CCSD.NET].SDM.[BI].[mv_GradeDistribution_HS] AS gd
	INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimCourse cr ON gd.CourseCode = cr.CourseCode 
	WHERE 
		cr.CourseCurrentRowIndicator = 'Current' AND
		gd.schoolYear = @ActiveYear
		AND @CurrentQtr IN ( 'Q2', 'Q4', 'Q1') 
		AND CHARINDEX('Progress Grade', gd.SchoolYearTermCourseGradeTypeDescription)=0
		AND gd.SchoolYearTermCourseGradeTypeDescription IN ( @HS_Grade_Semester_Description ) --( '2021-2022, S1, Quarter Grade', '2021-2022, Q1, Quarter Grade' )				
		AND cr.NCESSubjectCode IN ( '01', '02', '03', '04', '53', '54' )
	GROUP BY gd.EducationOrganizationId, gd.SchoolYearTermCourseGradeTypeDescription
) L1 ;


SELECT * FROM SDM.dbo.DimEducationOrganization WHERE EducationOrganizationCurrentRowIndicator = 'Current' 
AND CHARINDEX('Whitney', EducationOrganizationName ) > 0 


SELECT DISTINCT term FROM #gradesCounts
SELECT COUNT(*), COUNT(DISTINCT EducationOrganizationID) FROM #gradesCounts
SELECT * FROM (
	SELECT *, COUNT(*) OVER ( PARTITION BY EducationOrganizationID, schoolType ) cnt FROM #gradesCounts
) L1 WHERE cnt > 1 
ORDER BY EducationOrganizationID

SELECT * FROM #gradesCounts 
WHERE EducationOrganizationId  = '924' 


DROP TABLE #GradeDistList;

SELECT *	
INTO #GradeDistList
FROM (		
	SELECT 
		SchoolType, 
		term, 
		EducationOrganizationID AS schoolID, 
		ISNULL([letterACnt], '') AS Qtr_letterACnt, 
		ISNULL([letterBCnt], '') AS Qtr_letterBCnt, 
		ISNULL([letterCCnt], '') AS Qtr_letterCCnt, 
		ISNULL([letterDCnt], '') AS Qtr_letterDCnt, 
		ISNULL([letterFCnt], '') AS Qtr_letterFCnt, 
		ISNULL([letterECnt], '') AS Qtr_letterECnt, 
		ISNULL([letterSCnt], '') AS Qtr_letterSCnt, 
		ISNULL([letterNCnt], '') AS Qtr_letterNCnt, 
		ISNULL([letter1Cnt], '') AS Qtr_letter1Cnt, 
		ISNULL([letter2Cnt], '') AS Qtr_letter2Cnt, 
		ISNULL([letter3Cnt], '') AS Qtr_letter3Cnt, 
		ISNULL([letter4Cnt], '') AS Qtr_letter4Cnt
	FROM #gradesCounts 
	WHERE term IN ( 'Q1', 'Q2', 'Q3', 'Q4' ) 
) L1 OUTER APPLY
( 
	SELECT 		
		ISNULL([letterACnt], '') AS Semester_letterACnt, 
		ISNULL([letterBCnt], '') AS Semester_letterBCnt, 
		ISNULL([letterCCnt], '') AS Semester_letterCCnt, 
		ISNULL([letterDCnt], '') AS Semester_letterDCnt, 
		ISNULL([letterFCnt], '') AS Semester_letterFCnt, 
		ISNULL([letterECnt], '') AS Semester_letterECnt, 
		ISNULL([letterSCnt], '') AS Semester_letterSCnt, 
		ISNULL([letterNCnt], '') AS Semester_letterNCnt, 
		ISNULL([letter1Cnt], '') AS Semester_letter1Cnt, 
		ISNULL([letter2Cnt], '') AS Semester_letter2Cnt, 
		ISNULL([letter3Cnt], '') AS Semester_letter3Cnt, 
		ISNULL([letter4Cnt], '') AS Semester_letter4Cnt
	FROM #gradesCounts g2
	WHERE term IN ( 'S1', 'S2' ) 
		AND L1.schoolID = g2.EducationOrganizationId AND L1.schoolType = g2.schoolType 
) L2 
;


SELECT * FROM #GradeDistList





DROP TABLE #OnTrackBySchool;
DROP TABLE #OnTrackBySchoolCnts;
DROP TABLE #OnTrackList

DECLARE @Year1 INT = 2022;
DECLARE @Year2 INT = 2023;
DECLARE @Year3 INT = 2024;
DECLARE @Year4 INT = 2025;

DECLARE @HealthCredits VARCHAR(20) = 'Include';
DECLARE @InProgress VARCHAR(20) = 'Exclude' ;

SELECT 
	[AB3_SchoolId] AS schoolID, 
	schoolType, 	
	CASE AB2_ClassOf
		WHEN @Year1 THEN 'Year1'
		WHEN @Year2 THEN 'Year2' 
		WHEN @Year3 THEN 'Year3' 
		WHEN @Year4 THEN 'Year4'
	END AS ClassOf,				
	OnTrackStatus, SUM(NumberOfRecords) cnt 
INTO #OnTrackBySchoolCnts
FROM (
	SELECT 
		--CASE [Health & PE Credits]
		CASE @HealthCredits
			WHEN 'Include' THEN
				CASE @InProgress
					WHEN 'Include' THEN [AF5_OnTrackStatusInProgress]
					WHEN 'Exclude' THEN AF1_OnTrackStatus
				END
			WHEN 'Exclude' THEN
				CASE @InProgress
					WHEN 'Include' THEN [AF7_OnTrackStatusPEHealthExcludeInProgress]
					WHEN 'Exclude' THEN [AF6_OnTrackStatusPEHealthExclude]
				END
		END OnTrackStatus,
		CASE 
			WHEN [AD2_GradeLevelTypeDescription] IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' 
			WHEN [AD2_GradeLevelTypeDescription] IN ('06', '07', '08' ) THEN 'MS' 
			WHEN [AD2_GradeLevelTypeDescription] IN ( 
				'12th Grade', 'Adult Education', '10th Grade', 'Ungraded', '9th Grade', '11th Grade'
			) THEN 'HS' 
		END AS schoolType,
		1 AS NumberOfRecords,
	* FROM 
	(
		SELECT 
			[v_mv_StudentCohortOnTrack_TabSort].[AA1_StudentNumber] AS [AA1_StudentNumber],
			[v_mv_StudentCohortOnTrack_TabSort].[AA2_LastName] AS [AA2_LastName],
			[v_mv_StudentCohortOnTrack_TabSort].[AA3_FirstName] AS [AA3_FirstName],
			[v_mv_StudentCohortOnTrack_TabSort].[AB2_ClassOf] AS [AB2_ClassOf],
			[v_mv_StudentCohortOnTrack_TabSort].[AB3_SchoolId] AS [AB3_SchoolId],
			[v_mv_StudentCohortOnTrack_TabSort].[AB4_SchoolName] AS [AB4_SchoolName],
			[v_mv_StudentCohortOnTrack_TabSort].[AB5_PerformanceZoneCode] AS [AB5_PerformanceZoneCode],
			[v_mv_StudentCohortOnTrack_TabSort].[AB6_PerformanceZoneName] AS [AB6_PerformanceZoneName],
			[v_mv_StudentCohortOnTrack_TabSort].[AC1_GenderDescription] AS [AC1_GenderDescription],
			[v_mv_StudentCohortOnTrack_TabSort].[AC2_RaceTypeCodeValue] AS [AC2_RaceTypeCodeValue],
			[v_mv_StudentCohortOnTrack_TabSort].[AC3_RaceTypeDescription] AS [AC3_RaceTypeDescription],
			[v_mv_StudentCohortOnTrack_TabSort].[AC4_IsLEP] AS [AC4_IsLEP],
			[v_mv_StudentCohortOnTrack_TabSort].[AC7_IsIEP] AS [AC7_IsIEP],
			[v_mv_StudentCohortOnTrack_TabSort].[AC8_Is504] AS [AC8_Is504],
			[v_mv_StudentCohortOnTrack_TabSort].[AC9_IsMagnet] AS [AC9_IsMagnet],
			[v_mv_StudentCohortOnTrack_TabSort].[ACA1_CurrentMagnetProgram] AS [ACA1_CurrentMagnetProgram],
			[v_mv_StudentCohortOnTrack_TabSort].[AD1_GradeLevelTypeCodeValue] AS [AD1_GradeLevelTypeCodeValue], 
			[v_mv_StudentCohortOnTrack_TabSort].[AD2_GradeLevelTypeDescription] AS [AD2_GradeLevelTypeDescription],
			[v_mv_StudentCohortOnTrack_TabSort].[AD7_ActivelyEnrolledAtCohortSchool] AS [AD7_ActivelyEnrolledAtCohortSchool],
			[v_mv_StudentCohortOnTrack_TabSort].[AD8_UnsuccessfulTransferAtCohortSchool] AS [AD8_UnsuccessfulTransferAtCohortSchool],
			[v_mv_StudentCohortOnTrack_TabSort].[AD9_OutOfDistrictReceivingSchoolName] AS [AD9_OutOfDistrictReceivingSchoolName],
			[v_mv_StudentCohortOnTrack_TabSort].[ADA1_OutOfDistrictReceivingSchoolDate] AS [ADA1_OutOfDistrictReceivingSchoolDate],
			[v_mv_StudentCohortOnTrack_TabSort].[AF1_OnTrackStatus] AS [AF1_OnTrackStatus],
			[v_mv_StudentCohortOnTrack_TabSort].[AF2_OnTrackCredits] AS [AF2_OnTrackCredits],
			[v_mv_StudentCohortOnTrack_TabSort].[AF3_OnTrackProficiency] AS [AF3_OnTrackProficiency],
			[v_mv_StudentCohortOnTrack_TabSort].[AF5_OnTrackStatusInProgress] AS [AF5_OnTrackStatusInProgress],
			[v_mv_StudentCohortOnTrack_TabSort].[AF6_OnTrackStatusPEHealthExclude] AS [AF6_OnTrackStatusPEHealthExclude],
			[v_mv_StudentCohortOnTrack_TabSort].[AF7_OnTrackStatusPEHealthExcludeInProgress] AS [AF7_OnTrackStatusPEHealthExcludeInProgress],
			[v_mv_StudentCohortOnTrack_TabSort].[AG3_OnTrackCreditsInProgress] AS [AG3_OnTrackCreditsInProgress],
			[v_mv_StudentCohortOnTrack_TabSort].[AG4_OnTrackCreditsPEHealthExclude] AS [AG4_OnTrackCreditsPEHealthExclude],
			[v_mv_StudentCohortOnTrack_TabSort].[AG5_OnTrackCreditsInProgressPEHealthExclude] AS [AG5_OnTrackCreditsInProgressPEHealthExclude]
		FROM [ORION.CIS.CCSD.NET].SDM.[BI].[v_mv_StudentCohortOnTrack_TabSort] [v_mv_StudentCohortOnTrack_TabSort] 
		INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimEducationOrganization sch ON LTRIM(RTRIM(CAST([v_mv_StudentCohortOnTrack_TabSort].AB3_schoolID AS VARCHAR(20)))) = LTRIM(RTRIM(sch.EducationOrganizationID))
			AND sch.EducationOrganizationCurrentRowIndicator = 'Current'
	) L1 
) L2
WHERE 	
	[AB2_ClassOf] IN ( @Year1, @Year2, @Year3, @Year4 )
GROUP BY [AB3_SchoolId], schoolType, AB2_ClassOf, OnTrackStatus	
ORDER BY AB2_ClassOf, CASE onTrackStatus WHEN 'Y' THEN 1 WHEN 'N' THEN 2 WHEN 'U' THEN 3 END ASC;


SELECT 
	schoolID, schoolType, ClassOf, [Y], [Ttl], FORMAT(ROUND(OnTrackPerc, 4), 'N4') AS OnTrackPerc
INTO #OnTrackBySchool
FROM 
(
	SELECT schoolID, schoolType, ClassOf, [Y], [Ttl], CAST([Y] AS DECIMAL(15, 5)) / CAST([Ttl] AS DECIMAL(15, 5)) OnTrackPerc FROM (
		SELECT 
			schoolID, schoolType, 
			ClassOf,			
			ISNULL([Y], 0) AS [Y], ISNULL([N], 0) AS [N], ISNULL([U], 0) AS [U], ISNULL([Y], 0) + ISNULL([N], 0) + ISNULL([U], 0) AS [Ttl]
		FROM
		( SELECT * FROM #OnTrackBySchoolCnts ) mv
		PIVOT ( MAX(cnt) FOR OnTrackStatus IN ( [Y], [N], [U] ) ) pv 
	) L1 
) L2;

SELECT schoolType, schoolID, [Year1_OnTrack], [Year2_OnTrack], [Year3_OnTrack], [Year4_OnTrack]
INTO #OnTrackList
FROM ( 
	SELECT schoolType, schoolID, CONCAT(ClassOf, '_OnTrack') AS CntType, OnTrackPerc FROM #OnTrackBySchool WHERE schoolID > 0 	
) mv
PIVOT ( MAX(OnTrackPerc) FOR CntType IN ( [Year1_OnTrack], [Year2_OnTrack], [Year3_OnTrack], [Year4_OnTrack] ) ) pv 
ORDER BY schoolID, schoolType;

GO


/* AP IB DC CTE */
--DECLARE @ActiveYear int = (	SELECT SchoolYear FROM SDM.curr.DimSchoolYear WHERE SchoolYearActiveIndicator = 'Active' );
--DECLARE @ValidationDate int = ( SELECT CAST( CAST(@ActiveYear-1 AS varchar) + '1001' AS int) );

--===============================================================
-- Current run time: 4 minutes
--

/*IF OBJECT_ID('tempdb..#mainRosterData') IS NOT NULL
	DROP TABLE #mainRosterData

SELECT
	EducationOrganizationID
	, EducationOrganizationStateID
	, EducationOrganizationShortName
	, SchoolCategoryDescription
	, AcademicOrganizationalUnitCode
	, StudentNumber
	, StudentLastName + ', ' + StudentFirstName AS StudentName
	, StudentGenderCode
	, StudentGenderDescription
	, StudentEthnicityDescription
	, StudentEthnicityFederalDescription
	--, GradeLevelCode
	, ClassOf
	, IEPIndicator
	, LEPIndicator
	, Section504Indicator
	, SchoolEndYear
	, EntryDateEducationOrganizationDate
	, EntryFullDate
	, ExitFullDate
	, CourseCode
	, CourseTitle
	, CourseLevelCode
	, CourseLevelDescription
	, ActivelyEnrolled
	, ActivelyRosteredIndicator
	, CurrentCourse = IIF( CAST(GETDATE() AS date) BETWEEN entryfulldate AND exitfulldate, 1, 0)
	, APIndicator = IIF( SUBSTRING(CourseCode, 5, 1) = '2', 1, 0)
	, DualCreditIndicator = IIF((
			CourseCode LIKE '9%' OR CourseCode LIKE '8014%' OR CourseCode LIKE '1451%' OR CourseCode LIKE '7394%' OR CourseCode LIKE '42414%' OR CourseCode LIKE '4539%' OR CourseCode LIKE '44292%' OR CourseCode LIKE '4546%' OR CourseCode LIKE '4547%' OR CourseCode LIKE '3105%' OR CourseCode LIKE '3180%' OR CourseCode LIKE '21464%' OR CourseCode LIKE '215640%' OR CourseCode LIKE '3105%' OR CourseCode LIKE '3180%' OR CourseCode LIKE '1180%' OR CourseCode LIKE '11904%' OR CourseCode LIKE '12004%' OR CourseCode LIKE '41704%' OR CourseCode LIKE '41704%' OR CourseCode LIKE '8036%' OR CourseCode LIKE '8037%' OR CourseCode LIKE '8038%' OR CourseCode LIKE '2300%' OR CourseCode LIKE '2310%' OR CourseCode LIKE '7393%' OR CourseCode LIKE '2285%' OR CourseCode LIKE '2280%' OR CourseCode LIKE '7396%' OR CourseCode LIKE '42412%' OR CourseCode LIKE '1452%' OR CourseCode 
			LIKE '3271%' OR CourseCode LIKE '4170%' OR CourseCode LIKE '6270%' OR CourseCode LIKE '6491%' OR CourseCode LIKE '6492%' OR CourseCode LIKE '6493%' OR CourseCode LIKE '6494%' OR CourseCode LIKE '6803%' OR CourseCode LIKE '6808%' OR CourseCode LIKE '6823%' OR CourseCode LIKE '7392%' OR CourseCode LIKE '7601%' OR CourseCode LIKE '80300%' OR CourseCode LIKE '30614%' OR CourseCode LIKE '41714%' OR CourseCode LIKE '4645%' OR CourseCode LIKE '21662%'
			) AND LEN(CourseCode) >= 5, 1, 0)
	, CTEIndicator = IIF(((CourseCode LIKE '6%' OR CourseCode LIKE '5388%' OR CourseCode LIKE '5392%' OR CourseCode LIKE '5396%' OR CourseCode LIKE '5560%' OR CourseCode LIKE '5570%' OR CourseCode LIKE '5580%' OR CourseCode LIKE '5304%' OR CourseCode LIKE '5310%' OR CourseCode LIKE '5340%' OR CourseCode LIKE '5344%' OR CourseCode LIKE '5348%' OR CourseCode LIKE '5352%' OR CourseCode LIKE '5356%' OR CourseCode LIKE '5510%' OR CourseCode LIKE '5520%' OR CourseCode LIKE '5530%' OR CourseCode LIKE '5372%' OR CourseCode LIKE '5376%' OR CourseCode LIKE '53804%' OR CourseCode LIKE '53844%' OR CourseCode LIKE '5385%' OR CourseCode LIKE '5370%' OR CourseCode LIKE '5373%' OR CourseCode LIKE '5374%' OR CourseCode LIKE '5397%' OR CourseCode LIKE '5486%' OR CourseCode LIKE '5487%' OR CourseCode LIKE '5490%' OR CourseCode LIKE '5491%' OR CourseCode LIKE '5494%' OR CourseCode LIKE '5495%' OR CourseCode LIKE '5391%'
	-- Added 2021-02-02 - Added due to additions found on IC Academic Plan for "18+ College and Career Diploma"
	OR CourseCode LIKE '5498%' OR CourseCode LIKE '5499%' OR CourseCode LIKE '53944%' OR CourseCode LIKE '5492%' OR CourseCode LIKE '54924%' OR CourseCode LIKE '5488%' OR CourseCode LIKE '54884%' OR CourseCode LIKE '5496%' OR CourseCode LIKE '54964%' OR CourseCode LIKE '5500%' OR CourseCode LIKE '55004%' OR CourseCode LIKE '5393%') AND CourseCode NOT LIKE '6013%' AND CourseCode NOT LIKE '6015%' AND CourseCode NOT LIKE '6016%' AND CourseCode NOT LIKE '69%'), 1, 0)
	, IBIndicator = IIF( SUBSTRING(CourseCode, 5, 1) = '3', 1, 0)
	--, ValidationDayIndicator = IIF ( EntryDateKey <= @ValidationDate AND ExitDateKey >= @ValidationDate, 1, 0 )
INTO #mainRosterData
FROM [ORION.CIS.CCSD.NET].SDM.curr.FactRosterCombined frc
WHERE 1=1
	AND SchoolYear = @ActiveYear
	-- Course Type Restriction
	AND ( 
		-- AP Courses
		( SUBSTRING(CourseCode, 5, 1) = '2' AND CourseLevelCode = 'AP' )
		-- IB Courses
		OR  ( SUBSTRING(CourseCode, 5, 1) = '3' AND LEN(CourseCode) >= 8 AND CourseLevelCode = 'IB' )
		-- Dual Credit Courses
		OR ( (
		CourseCode LIKE '9%' OR CourseCode LIKE '8014%' OR CourseCode LIKE '1451%' OR CourseCode LIKE '7394%' OR CourseCode LIKE '42414%' OR CourseCode LIKE '4539%' OR CourseCode LIKE '44292%' OR CourseCode LIKE '4546%' OR CourseCode LIKE '4547%' OR CourseCode LIKE '3105%' OR CourseCode LIKE '3180%' OR CourseCode LIKE '21464%' OR CourseCode LIKE '215640%' OR CourseCode LIKE '3105%' OR CourseCode LIKE '3180%' OR CourseCode LIKE '1180%' OR CourseCode LIKE '11904%' OR CourseCode LIKE '12004%' OR CourseCode LIKE '41704%' OR CourseCode LIKE '41704%' OR CourseCode LIKE '8036%' OR CourseCode LIKE '8037%' OR CourseCode LIKE '8038%' OR CourseCode LIKE '2300%' OR CourseCode LIKE '2310%' OR CourseCode LIKE '7393%' OR CourseCode LIKE '2285%' OR CourseCode LIKE '2280%' OR CourseCode LIKE '7396%' OR CourseCode LIKE '42412%' OR CourseCode LIKE '1452%' OR CourseCode 
			LIKE '3271%' OR CourseCode LIKE '4170%' OR CourseCode LIKE '6270%' OR CourseCode LIKE '6491%' OR CourseCode LIKE '6492%' OR CourseCode LIKE '6493%' OR CourseCode LIKE '6494%' OR CourseCode LIKE '6803%' OR CourseCode LIKE '6808%' OR CourseCode LIKE '6823%' OR CourseCode LIKE '7392%' OR CourseCode LIKE '7601%' OR CourseCode LIKE '80300%' OR CourseCode LIKE '30614%' OR CourseCode LIKE '41714%' OR CourseCode LIKE '4645%' OR CourseCode LIKE '21662%'
			) AND LEN(CourseCode) >= 5 )
		-- CTE Courses
	OR ((CourseCode LIKE '6%' OR CourseCode LIKE '5388%' OR CourseCode LIKE '5392%' OR CourseCode LIKE '5396%' OR CourseCode LIKE '5560%' OR CourseCode LIKE '5570%' OR CourseCode LIKE '5580%' OR CourseCode LIKE '5304%' OR CourseCode LIKE '5310%' OR CourseCode LIKE '5340%' OR CourseCode LIKE '5344%' OR CourseCode LIKE '5348%' OR CourseCode LIKE '5352%' OR CourseCode LIKE '5356%' OR CourseCode LIKE '5510%' OR CourseCode LIKE '5520%' OR CourseCode LIKE '5530%' OR CourseCode LIKE '5372%' OR CourseCode LIKE '5376%' OR CourseCode LIKE '53804%' OR CourseCode LIKE '53844%' OR CourseCode LIKE '5385%' OR CourseCode LIKE '5370%' OR CourseCode LIKE '5373%' OR CourseCode LIKE '5374%' OR CourseCode LIKE '5397%' OR CourseCode LIKE '5486%' OR CourseCode LIKE '5487%' OR CourseCode LIKE '5490%' OR CourseCode LIKE '5491%' OR CourseCode LIKE '5494%' OR CourseCode LIKE '5495%' OR CourseCode LIKE '5391%'
	-- Added 2021-02-02 - Added due to additions found on IC Academic Plan for "18+ College and Career Diploma"
	OR CourseCode LIKE '5498%' OR CourseCode LIKE '5499%' OR CourseCode LIKE '53944%' OR CourseCode LIKE '5492%' OR CourseCode LIKE '54924%' OR CourseCode LIKE '5488%' OR CourseCode LIKE '54884%' OR CourseCode LIKE '5496%' OR CourseCode LIKE '54964%' OR CourseCode LIKE '5500%' OR CourseCode LIKE '55004%' OR CourseCode LIKE '5393%') AND CourseCode NOT LIKE '6013%' AND CourseCode NOT LIKE '6015%' AND CourseCode NOT LIKE '6016%' AND CourseCode NOT LIKE '69%')
)*/



DECLARE @ActiveYear int = (	SELECT SchoolYear FROM SDM.curr.DimSchoolYear WHERE SchoolYearActiveIndicator = 'Active' );
DECLARE @ValidationDate int = ( SELECT CAST( CAST(@ActiveYear-1 AS varchar) + '1001' AS int) );

DECLARE @Year1 INT = 2022;
DECLARE @Year2 INT = 2023;
DECLARE @Year3 INT = 2024;
DECLARE @Year4 INT = 2025;

DROP TABLE #CurrentlyEnrolledStudents;
SELECT DISTINCT 
	CASE WHEN mv.GradeLevelCode IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN GradeLevelCode IN ('06', '07', '08' ) THEN 'MS' WHEN GradeLevelCode IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType,
	mv.EducationOrganizationID, mv.StudentEthnicityCode AS cntType, 
	mv.StudentNumber, 
	mvt.ClassOf
INTO #CurrentlyEnrolledStudents
FROM [ORION.CIS.CCSD.NET].SDM.Dif.mv_Enrollment mv 
INNER JOIN [ORION.CIS.CCSD.NET].SDM.[BI].[mv_StudentCohortOnTrack] mvt ON mv.StudentNumber = mvt.StudentUSI
WHERE 
	mv.SchoolYear = @ActiveYear 
	AND mvt.SchoolYear = @ActiveYear 
	AND mvt.ClassOf IN ( @Year1, @Year2, @Year3, @Year4 ) 
	AND SchoolYearActiveIndicator = 'Active' 
	AND CurrentEnrollment = 1	
	AND StudentEthnicityCode IN ( 'A', 'B', 'C', 'H', 'I', 'M', 'P' ) 
;


DROP TABLE #CohortsCnt;

SELECT 
	schoolType, schoolID, [Year1], [Year2], [Year3], [Year4]
INTO #CohortsCnt
FROM 
(
	SELECT 
		schoolType, schoolID, YearID, COUNT(DISTINCT StudentNumber) cnt
	FROM (
		SELECT 
			mv.EducationOrganizationID AS schoolID, mv.studentNumber, 
			mv.ClassOf, 
			CASE mv.ClassOf WHEN @Year1 THEN 'Year1' WHEN @Year2 THEN 'Year2' WHEN @Year3 THEN 'Year3' WHEN @Year4 THEN 'Year4' END YearID,		
			mv.schoolType
		FROM #CurrentlyEnrolledStudents AS mv
		WHERE 
			mv.ClassOf IN ( @Year1, @Year2, @Year3, @Year4 ) 
	) L1
	GROUP BY schoolType, schoolID, ClassOf, YearID
) mv 
PIVOT ( MAX(cnt) FOR YearID IN ( [Year1], [Year2], [Year3], [Year4] ) ) pv
;


/*
DECLARE @Year1 INT = 2022;
DECLARE @Year2 INT = 2023;
DECLARE @Year3 INT = 2024;
DECLARE @Year4 INT = 2025;
SELECT 
	schoolType, schoolID, [Year1], [Year2], [Year3], [Year4]
INTO #CohortsCnt_UnmatchedToCurrentEnrolls
FROM 
(
	SELECT 
		schoolType, schoolID, YearID, COUNT(DISTINCT StudentUSI) cnt
	FROM (
		SELECT 
			mv.schoolID, mv.studentUSI, 
			mv.ClassOf, 
			CASE mv.ClassOf WHEN @Year1 THEN 'Year1' WHEN @Year2 THEN 'Year2' WHEN @Year3 THEN 'Year3' WHEN @Year4 THEN 'Year4' END YearID,		
			s.StudentGradeLevelCode AS GradeLevelCode, 			
			CASE WHEN s.StudentGradeLevelCode IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN s.StudentGradeLevelCode  IN ('06', '07', '08' ) THEN 'MS' WHEN s.StudentGradeLevelCode IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType
		FROM [ORION.CIS.CCSD.NET].SDM.[BI].[mv_StudentCohortOnTrack] mv
		INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimStudent s ON mv.StudentUSI = s.studentNumber 
		WHERE 
			s.StudentCurrentRowIndicator = 'Current' 
			AND s.ActivelyEnrolled = 'Actively Enrolled' 
			AND mv.ClassOf IN ( @Year1, @Year2, @Year3, @Year4 ) 
			--AND mv.OnTrackStatus = 'Y'
	) L1
	GROUP BY schoolType, schoolID, ClassOf, YearID
) mv 
PIVOT ( MAX(cnt) FOR YearID IN ( [Year1], [Year2], [Year3], [Year4] ) ) pv
;*/

SELECT 	
	schoolType, schoolID, studentUSI, ClassOf, YearID, GradeLevelCode
INTO #CohortsList
FROM 
(	
	SELECT DISTINCT
		mv.schoolID, mv.studentUSI, 
		mv.ClassOf, 
		CASE mv.ClassOf WHEN @Year1 THEN 'Year1' WHEN @Year2 THEN 'Year2' WHEN @Year3 THEN 'Year3' WHEN @Year4 THEN 'Year4' END YearID,		
		s.StudentGradeLevelCode AS GradeLevelCode, 			
		CASE WHEN s.StudentGradeLevelCode IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN s.StudentGradeLevelCode  IN ('06', '07', '08' ) THEN 'MS' WHEN s.StudentGradeLevelCode IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType
	FROM [ORION.CIS.CCSD.NET].SDM.[BI].[mv_StudentCohortOnTrack] mv
	INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimStudent s ON mv.StudentUSI = s.studentNumber 
	WHERE 
		s.StudentCurrentRowIndicator = 'Current' 
		AND s.ActivelyEnrolled = 'Actively Enrolled' 
		AND mv.ClassOf IN ( @Year1, @Year2, @Year3, @Year4 ) 
		--AND mv.OnTrackStatus = 'Y'	
) L1 
;


IF OBJECT_ID('tempdb..#acceleratedTest') IS NOT NULL DROP TABLE #acceleratedTest;

SELECT
	frc.EducationOrganizationID
	, ed.EducationOrganizationStateID
	, ed.EducationOrganizationShortName
	, ed.SchoolCategoryDescription
	, ed.AcademicOrganizationalUnitCode
	, frc.StudentNumber
	, StudentLastName + ', ' + StudentFirstName AS StudentName
	, StudentGenderCode
	, StudentGenderDescription
	, StudentEthnicityDescription
	, StudentEthnicityFederalDescription
	--, A1.GradeLevelCode
	--, A1.GradeLevelDescription
	--, GradeLevelCode
	, ClassOf
	, IEPIndicator
	, LEPIndicator
	, Section504Indicator
	, frc.SchoolYear AS SchoolEndYear
	--, EntryDateEducationOrganizationDate
	, entryDate.fulldate as EntryFullDate
	, exitDate.fullDate AS ExitFullDate
	, dc.CourseCode
	, dc.CourseTitle
	, dc.CourseLevelCode
	, dc.CourseLevelDescription
	, ds.ActivelyEnrolled
	--, ActivelyRosteredIndicator
	, CurrentCourse = IIF( CAST(GETDATE() AS date) BETWEEN entryDate.fulldate AND exitDate.fullDate, 1, 0)
	, APIndicator = IIF( SUBSTRING(CourseCode, 5, 1) = '2', 1, 0)
	, DualCreditIndicator = IIF((
			CourseCode LIKE '9%' OR CourseCode LIKE '8014%' OR CourseCode LIKE '1451%' OR CourseCode LIKE '7394%' OR CourseCode LIKE '42414%' OR CourseCode LIKE '4539%' OR CourseCode LIKE '44292%' OR CourseCode LIKE '4546%' OR CourseCode LIKE '4547%' OR CourseCode LIKE '3105%' OR CourseCode LIKE '3180%' OR CourseCode LIKE '21464%' OR CourseCode LIKE '215640%' OR CourseCode LIKE '3105%' OR CourseCode LIKE '3180%' OR CourseCode LIKE '1180%' OR CourseCode LIKE '11904%' OR CourseCode LIKE '12004%' OR CourseCode LIKE '41704%' OR CourseCode LIKE '41704%' OR CourseCode LIKE '8036%' OR CourseCode LIKE '8037%' OR CourseCode LIKE '8038%' OR CourseCode LIKE '2300%' OR CourseCode LIKE '2310%' OR CourseCode LIKE '7393%' OR CourseCode LIKE '2285%' OR CourseCode LIKE '2280%' OR CourseCode LIKE '7396%' OR CourseCode LIKE '42412%' OR CourseCode LIKE '1452%' OR CourseCode 
			LIKE '3271%' OR CourseCode LIKE '4170%' OR CourseCode LIKE '6270%' OR CourseCode LIKE '6491%' OR CourseCode LIKE '6492%' OR CourseCode LIKE '6493%' OR CourseCode LIKE '6494%' OR CourseCode LIKE '6803%' OR CourseCode LIKE '6808%' OR CourseCode LIKE '6823%' OR CourseCode LIKE '7392%' OR CourseCode LIKE '7601%' OR CourseCode LIKE '80300%' OR CourseCode LIKE '30614%' OR CourseCode LIKE '41714%' OR CourseCode LIKE '4645%' OR CourseCode LIKE '21662%'
			) AND LEN(CourseCode) >= 5, 1, 0)
	, CTEIndicator = IIF(((CourseCode LIKE '6%' OR CourseCode LIKE '5388%' OR CourseCode LIKE '5392%' OR CourseCode LIKE '5396%' OR CourseCode LIKE '5560%' OR CourseCode LIKE '5570%' OR CourseCode LIKE '5580%' OR CourseCode LIKE '5304%' OR CourseCode LIKE '5310%' OR CourseCode LIKE '5340%' OR CourseCode LIKE '5344%' OR CourseCode LIKE '5348%' OR CourseCode LIKE '5352%' OR CourseCode LIKE '5356%' OR CourseCode LIKE '5510%' OR CourseCode LIKE '5520%' OR CourseCode LIKE '5530%' OR CourseCode LIKE '5372%' OR CourseCode LIKE '5376%' OR CourseCode LIKE '53804%' OR CourseCode LIKE '53844%' OR CourseCode LIKE '5385%' OR CourseCode LIKE '5370%' OR CourseCode LIKE '5373%' OR CourseCode LIKE '5374%' OR CourseCode LIKE '5397%' OR CourseCode LIKE '5486%' OR CourseCode LIKE '5487%' OR CourseCode LIKE '5490%' OR CourseCode LIKE '5491%' OR CourseCode LIKE '5494%' OR CourseCode LIKE '5495%' OR CourseCode LIKE '5391%'
	-- Added 2021-02-02 - Added due to additions found on IC Academic Plan for "18+ College and Career Diploma"
	OR CourseCode LIKE '5498%' OR CourseCode LIKE '5499%' OR CourseCode LIKE '53944%' OR CourseCode LIKE '5492%' OR CourseCode LIKE '54924%' OR CourseCode LIKE '5488%' OR CourseCode LIKE '54884%' OR CourseCode LIKE '5496%' OR CourseCode LIKE '54964%' OR CourseCode LIKE '5500%' OR CourseCode LIKE '55004%' OR CourseCode LIKE '5393%') AND CourseCode NOT LIKE '6013%' AND CourseCode NOT LIKE '6015%' AND CourseCode NOT LIKE '6016%' AND CourseCode NOT LIKE '69%'), 1, 0)
	, IBIndicator = IIF( SUBSTRING(CourseCode, 5, 1) = '3', 1, 0 )
INTO #acceleratedTest
FROM [ORION.CIS.CCSD.NET].SDM.curr.FactRoster frc
INNER JOIN [ORION.CIS.CCSD.NET].SDM.curr.DimEducationOrganization ed on ed.EducationOrganizationKey = frc.EducationOrganizationKey
INNER JOIN [ORION.CIS.CCSD.NET].SDM.curr.DimStudent ds on ds.StudentKey = frc.StudentKey
INNER JOIN [ORION.CIS.CCSD.NET].SDM.curr.DimCourse dc on dc.CourseKey = frc.CourseKey
INNER JOIN [ORION.CIS.CCSD.NET].SDM.curr.DimDate entryDate ON frc.EntryDateKey = entrydate.DateKey
INNER JOIN [ORION.CIS.CCSD.NET].SDM.curr.DimDate exitDate ON exitDate.DateKey = frc.ExitDateKey
WHERE SchoolYear = @ActiveYear
	AND ClassOf IN ( @Year1, @Year2, @Year3, @Year4 ) 
	--AND ed.MiddleSchoolIndicator = 'Middle School' -- commented but you might want to use this
	AND LEN(CourseCode) >= 7 -- Filter out elementary schools
	--AND SUBSTRING(CourseCode, 5, 1) = '3' -- Limit data to only IB courses
	AND ( 
		-- AP Courses
		( SUBSTRING(CourseCode, 5, 1) = '2' AND CourseLevelCode = 'AP' )
		-- IB Courses
		OR  ( SUBSTRING(CourseCode, 5, 1) = '3' AND LEN(CourseCode) >= 8 AND CourseLevelCode = 'IB' )
		-- Dual Credit Courses
		OR ( 
			( 
				CourseCode LIKE '9%' OR CourseCode LIKE '8014%' OR CourseCode LIKE '1451%' OR CourseCode LIKE '7394%' OR CourseCode LIKE '42414%' OR CourseCode LIKE '4539%' OR CourseCode LIKE '44292%' OR CourseCode LIKE '4546%' OR CourseCode LIKE '4547%' OR CourseCode LIKE '3105%' OR CourseCode LIKE '3180%' OR CourseCode LIKE '21464%' OR CourseCode LIKE '215640%' OR CourseCode LIKE '3105%' OR CourseCode LIKE '3180%' OR CourseCode LIKE '1180%' OR CourseCode LIKE '11904%' OR CourseCode LIKE '12004%' OR CourseCode LIKE '41704%' OR CourseCode LIKE '41704%' OR CourseCode LIKE '8036%' OR CourseCode LIKE '8037%' OR CourseCode LIKE '8038%' OR CourseCode LIKE '2300%' OR CourseCode LIKE '2310%' OR CourseCode LIKE '7393%' OR CourseCode LIKE '2285%' OR CourseCode LIKE '2280%' OR CourseCode LIKE '7396%' OR CourseCode LIKE '42412%' OR CourseCode LIKE '1452%' OR CourseCode 
				LIKE '3271%' OR CourseCode LIKE '4170%' OR CourseCode LIKE '6270%' OR CourseCode LIKE '6491%' OR CourseCode LIKE '6492%' OR CourseCode LIKE '6493%' OR CourseCode LIKE '6494%' OR CourseCode LIKE '6803%' OR CourseCode LIKE '6808%' OR CourseCode LIKE '6823%' OR CourseCode LIKE '7392%' OR CourseCode LIKE '7601%' OR CourseCode LIKE '80300%' OR CourseCode LIKE '30614%' OR CourseCode LIKE '41714%' OR CourseCode LIKE '4645%' OR CourseCode LIKE '21662%'
			) AND LEN(CourseCode) >= 5 )
		-- CTE Courses
		OR (
				(CourseCode LIKE '6%' OR CourseCode LIKE '5388%' OR CourseCode LIKE '5392%' OR CourseCode LIKE '5396%' OR CourseCode LIKE '5560%' OR CourseCode LIKE '5570%' OR CourseCode LIKE '5580%' OR CourseCode LIKE '5304%' OR CourseCode LIKE '5310%' OR CourseCode LIKE '5340%' OR CourseCode LIKE '5344%' OR CourseCode LIKE '5348%' OR CourseCode LIKE '5352%' OR CourseCode LIKE '5356%' OR CourseCode LIKE '5510%' OR CourseCode LIKE '5520%' OR CourseCode LIKE '5530%' OR CourseCode LIKE '5372%' OR CourseCode LIKE '5376%' OR CourseCode LIKE '53804%' OR CourseCode LIKE '53844%' OR CourseCode LIKE '5385%' OR CourseCode LIKE '5370%' OR CourseCode LIKE '5373%' OR CourseCode LIKE '5374%' OR CourseCode LIKE '5397%' OR CourseCode LIKE '5486%' OR CourseCode LIKE '5487%' OR CourseCode LIKE '5490%' OR CourseCode LIKE '5491%' OR CourseCode LIKE '5494%' OR CourseCode LIKE '5495%' OR CourseCode LIKE '5391%'
	-- Added 2021-02-02 - Added due to additions found on IC Academic Plan for "18+ College and Career Diploma"
		OR CourseCode LIKE '5498%' OR CourseCode LIKE '5499%' OR CourseCode LIKE '53944%' OR CourseCode LIKE '5492%' OR CourseCode LIKE '54924%' OR CourseCode LIKE '5488%' OR CourseCode LIKE '54884%' OR CourseCode LIKE '5496%' OR CourseCode LIKE '54964%' OR CourseCode LIKE '5500%' OR CourseCode LIKE '55004%' OR CourseCode LIKE '5393%') AND CourseCode NOT LIKE '6013%' AND CourseCode NOT LIKE '6015%' AND CourseCode NOT LIKE '6016%' AND CourseCode NOT LIKE '69%')
)
;


-- Add grade level to the course records
-- Execution Time: 1:23 minutes
IF OBJECT_ID('tempdb..#rosterDataWithGrade') IS NOT NULL
	DROP TABLE #rosterDataWithGrade

SELECT DISTINCT
	A1.GradeLevelCode
	, A1.GradeLevelDescription
	, frc.*
INTO #rosterDataWithGrade
FROM #acceleratedTest frc
OUTER APPLY (
	SELECT
		DimGradeLevel.GradeLevelCode
		, DimGradeLevel.GradeLevelDescription
	FROM (
		SELECT
			GradeLevelKey
			, ROW_NUMBER() OVER (PARTITION BY fse.SchoolYear, fse.EducationOrganizationID, fse.StudentNumber ORDER BY EntryDate.FullDate DESC) rn
		FROM SDM.SDM.FactStudentEnrollment fse
		INNER JOIN SDM.curr.DimDate EntryDate ON EntryDate.DateKey = fse.EntryDateKey
		INNER JOIN SDM.curr.DimDate ExitDate ON ExitDate.DateKey = fse.ExitDateKey
		WHERE fse.StudentNumber = frc.StudentNumber
			AND fse.EducationOrganizationID = frc.EducationOrganizationID
			AND fse.SchoolYear = frc.SchoolEndYear
			-- checks for the enrollment record and grade within the validation day window (can work before validation day)
			--AND EntryDate.DateKey <= ( @ValidationDate - ( (SchoolYear - @ActiveYear) * 10000) ) AND ExitDate.DateKey >= ( @ValidationDate - ( (SchoolYear - @ActiveYear) * 10000) )
		) O1
	INNER JOIN SDM.curr.DimGradeLevel ON DimGradeLevel.GradeLevelKey = O1.GradeLevelKey
	WHERE rn = 1
	) A1
;

DROP TABLE #Combined_APIBs ;
SELECT DISTINCT
	[GradeLevelCode]
	,[GradeLevelDescription]
	,[EducationOrganizationID]
	,[EducationOrganizationStateID]
	,[EducationOrganizationShortName]
	,[SchoolCategoryDescription]
	,[AcademicOrganizationalUnitCode]
	,[StudentNumber]
	,[StudentName]
	,[StudentGenderCode]
	,[StudentGenderDescription]
	,[StudentEthnicityDescription]
	,[StudentEthnicityFederalDescription]
	,[ClassOf]
	,[IEPIndicator]
	,[LEPIndicator]
	,[Section504Indicator]
	,[SchoolEndYear]
	,[CourseTitle]
	,[CourseLevelCode]
	,[CourseLevelDescription]
	,0 AS [ActivelyEnrolled]
	,0 AS [CurrentCourse]
	, DualCreditIndicator
	, CTEIndicator 
	, 0 AS SnapshotIndicator
	, APIndicator
	, IBIndicator
INTO #Combined_APIBs 
FROM #rosterDataWithGrade
WHERE 1=1 
	and SchoolEndYear = @ActiveYear
;



DROP TABLE #APIBDCCTE;
SELECT APC.*, 
	CASE 
		WHEN A1.GradeLevel IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' 
		WHEN A1.GradeLevel  IN ('06', '07', '08' ) THEN 'MS' 
		WHEN A1.GradeLevel IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' 
		ELSE 'HS'
	END AS schoolType
INTO #APIBDCCTE
FROM #Combined_APIBs AS APC
--FROM dbo.APIBCTEDCParticipants_20210915 APC
LEFT JOIN (
	  SELECT [TotalEnrollment_ValidationDay].[SchoolYear] AS [SchoolYear],
		[TotalEnrollment_ValidationDay].[TotalStudentCount] AS [TotalStudentCount],
		[TotalEnrollment_ValidationDay].[SchoolCode] AS [SchoolCode],
		CAST([TotalEnrollment_ValidationDay].[SchoolCode] as nvarchar) AS [$temp0],
		([TotalEnrollment_ValidationDay].[SchoolYear]) AS [$temp1]
	  FROM [ORION.CIS.CCSD.NET].SDM.[DIF].[TotalEnrollment_ValidationDay] [TotalEnrollment_ValidationDay]
) [t0] ON ((APC.[EducationOrganizationStateID] = [t0].[$temp0]) AND (APC.[SchoolEndYear] = [t0].[$temp1]))
OUTER APPLY (
	SELECT
		DimGradeLevel.GradeLevelCode AS GradeLevel 
		, DimGradeLevel.GradeLevelDescription AS GradeLevelDesc
	FROM (
		SELECT
			GradeLevelKey
			, ROW_NUMBER() OVER (PARTITION BY fse.SchoolYear, fse.EducationOrganizationID, fse.StudentNumber ORDER BY EntryDate.FullDate DESC) rn
		FROM SDM.SDM.FactStudentEnrollment fse
		INNER JOIN SDM.curr.DimDate EntryDate ON EntryDate.DateKey = fse.EntryDateKey
		INNER JOIN SDM.curr.DimDate ExitDate ON ExitDate.DateKey = fse.ExitDateKey
		WHERE fse.StudentNumber = APC.StudentNumber
			AND fse.EducationOrganizationID = APC.EducationOrganizationID
			AND fse.SchoolYear = APC.SchoolEndYear
			-- checks for the enrollment record and grade within the validation day window (can work before validation day)
			--AND EntryDate.DateKey <= ( @ValidationDate - ( (SchoolYear - @ActiveYear) * 10000) ) AND ExitDate.DateKey >= ( @ValidationDate - ( (SchoolYear - @ActiveYear) * 10000) )
		) O1
	INNER JOIN SDM.curr.DimGradeLevel ON DimGradeLevel.GradeLevelKey = O1.GradeLevelKey
	WHERE rn = 1
) A1
/* This is applied newly on November 2nd per Jose's instruction to match list of students with currently enrolled students. */ 
INNER JOIN #CurrentlyEnrolledStudents s ON APC.StudentNumber = s.StudentNumber AND APC.EducationOrganizationId = s.EducationOrganizationID AND APC.ClassOf = s.ClassOf
;
GO

SELECT * FROM(
SELECT DISTINCT studentNumber FROM #Combined_APIBs --WHERE studentNumber = '1279568'
EXCEPT
SELECT DISTINCT studentNumber FROM #CurrentlyEnrolledStudents --WHERE studentNumber = '1279568'
) L1 --WHERE studentNumber = '1279568' 



DECLARE @Year1 INT = 2022;
DECLARE @Year2 INT = 2023;
DECLARE @Year3 INT = 2024;
DECLARE @Year4 INT = 2025;

--SELECT schoolID, schoolType FROM #CohortsCnt
--EXCEPT
--SELECT DISTINCT EducationOrganizationID AS schoolID, schoolType FROM #APIBDCCTE


--SELECT * FROM SDM.dbo.DimEducationOrganization WHERE  EducationOrganizationCurrentRowIndicator = 'Current' AND 
--EducationOrganizationID IN ( 
--	SELECT schoolID FROM (
--	SELECT schoolID, schoolType FROM #CohortsCnt
--	EXCEPT
--	SELECT DISTINCT EducationOrganizationID AS schoolID, schoolType FROM #APIBDCCTE
--	) L1 
--) 

DROP TABLE #APIB_Cnts
SELECT 
	schoolType, EducationOrganizationID, 
	[Year1_AP], [Year1_IB], [Year1_DC], [Year1_CTE], [Year1_Overall], 	
	[Year2_AP], [Year2_IB], [Year2_DC], [Year2_CTE], [Year2_Overall], 
	[Year3_AP], [Year3_IB], [Year3_DC], [Year3_CTE], [Year3_Overall],
	[Year4_AP], [Year4_IB], [Year4_DC], [Year4_CTE], [Year4_Overall] 
INTO #APIB_Cnts
FROM
( 
	SELECT schoolType, EducationOrganizationID, cntType, cnt FROM (
		SELECT schoolType, EducationOrganizationID, CONCAT('Year1', '_', 'AP') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE APIndicator = 1 AND ClassOf  = @Year1 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year2', '_', 'AP') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE APIndicator = 1 AND ClassOf  = @Year2 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year3', '_', 'AP') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE APIndicator = 1 AND ClassOf  = @Year3 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year4', '_', 'AP') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE APIndicator = 1 AND ClassOf  = @Year4 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year1', '_', 'IB') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE IBIndicator = 1 AND ClassOf  = @Year1 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year2', '_', 'IB') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE IBIndicator = 1 AND ClassOf  = @Year2 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year3', '_', 'IB') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE IBIndicator = 1 AND ClassOf  = @Year3 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year4', '_', 'IB') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE IBIndicator = 1 AND ClassOf  = @Year4 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year1', '_', 'DC') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE DualCreditIndicator = 1 AND ClassOf  = @Year1 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year2', '_', 'DC') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE DualCreditIndicator = 1 AND ClassOf  = @Year2 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year3', '_', 'DC') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE DualCreditIndicator = 1 AND ClassOf  = @Year3 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year4', '_', 'DC') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE DualCreditIndicator = 1 AND ClassOf  = @Year4 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year1', '_', 'CTE') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE CTEIndicator = 1 AND ClassOf  = @Year1 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year2', '_', 'CTE') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE CTEIndicator = 1 AND ClassOf  = @Year2 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year3', '_', 'CTE') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE CTEIndicator = 1 AND ClassOf  = @Year3 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year4', '_', 'CTE') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE CTEIndicator = 1 AND ClassOf  = @Year4 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year1', '_', 'Overall') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE APIndicator = 1 OR IBIndicator = 1 OR DualCreditIndicator = 1 OR CTEIndicator = 1 AND ClassOf  = @Year1 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year2', '_', 'Overall') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE APIndicator = 1 OR IBIndicator = 1 OR DualCreditIndicator = 1 OR CTEIndicator = 1 AND ClassOf  = @Year2 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year3', '_', 'Overall') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE APIndicator = 1 OR IBIndicator = 1 OR DualCreditIndicator = 1 OR CTEIndicator = 1 AND ClassOf  = @Year3 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year4', '_', 'Overall') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE WHERE APIndicator = 1 OR IBIndicator = 1 OR DualCreditIndicator = 1 OR CTEIndicator = 1 AND ClassOf  = @Year4 GROUP BY schoolType, EducationOrganizationID
	) L0
) mv
PIVOT ( MAX(cnt) FOR 
		cntType IN ( 
			[Year1_AP], [Year1_IB], [Year1_DC], [Year1_CTE], [Year1_Overall], 
			[Year2_AP], [Year2_IB], [Year2_DC], [Year2_CTE], [Year2_Overall],
			[Year3_AP], [Year3_IB], [Year3_DC], [Year3_CTE], [Year3_Overall],
			[Year4_AP], [Year4_IB], [Year4_DC], [Year4_CTE], [Year4_Overall] ) 
) pv
;



DECLARE @Year1 INT = 2022;
DECLARE @Year2 INT = 2023;
DECLARE @Year3 INT = 2024;
DECLARE @Year4 INT = 2025;
DROP TABLE #APIBDCCTE_UnmatchedToCurrentEnrolls;
SELECT APC.*, 
	CASE 
		WHEN A1.GradeLevel IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' 
		WHEN A1.GradeLevel  IN ('06', '07', '08' ) THEN 'MS' 
		WHEN A1.GradeLevel IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' 
		ELSE 'HS'
	END AS schoolType
INTO #APIBDCCTE_UnmatchedToCurrentEnrolls
FROM #Combined_APIBs AS APC
--FROM dbo.APIBCTEDCParticipants_20210915 APC
LEFT JOIN (
	  SELECT [TotalEnrollment_ValidationDay].[SchoolYear] AS [SchoolYear],
		[TotalEnrollment_ValidationDay].[TotalStudentCount] AS [TotalStudentCount],
		[TotalEnrollment_ValidationDay].[SchoolCode] AS [SchoolCode],
		CAST([TotalEnrollment_ValidationDay].[SchoolCode] as nvarchar) AS [$temp0],
		([TotalEnrollment_ValidationDay].[SchoolYear]) AS [$temp1]
	  FROM [ORION.CIS.CCSD.NET].SDM.[DIF].[TotalEnrollment_ValidationDay] [TotalEnrollment_ValidationDay]
) [t0] ON ((APC.[EducationOrganizationStateID] = [t0].[$temp0]) AND (APC.[SchoolEndYear] = [t0].[$temp1]))
OUTER APPLY (
	SELECT
		DimGradeLevel.GradeLevelCode AS GradeLevel 
		, DimGradeLevel.GradeLevelDescription AS GradeLevelDesc
	FROM (
		SELECT
			GradeLevelKey
			, ROW_NUMBER() OVER (PARTITION BY fse.SchoolYear, fse.EducationOrganizationID, fse.StudentNumber ORDER BY EntryDate.FullDate DESC) rn
		FROM SDM.SDM.FactStudentEnrollment fse
		INNER JOIN SDM.curr.DimDate EntryDate ON EntryDate.DateKey = fse.EntryDateKey
		INNER JOIN SDM.curr.DimDate ExitDate ON ExitDate.DateKey = fse.ExitDateKey
		WHERE fse.StudentNumber = APC.StudentNumber
			AND fse.EducationOrganizationID = APC.EducationOrganizationID
			AND fse.SchoolYear = APC.SchoolEndYear
			-- checks for the enrollment record and grade within the validation day window (can work before validation day)
			--AND EntryDate.DateKey <= ( @ValidationDate - ( (SchoolYear - @ActiveYear) * 10000) ) AND ExitDate.DateKey >= ( @ValidationDate - ( (SchoolYear - @ActiveYear) * 10000) )
		) O1
	INNER JOIN SDM.curr.DimGradeLevel ON DimGradeLevel.GradeLevelKey = O1.GradeLevelKey
	WHERE rn = 1
) A1
--/* This is applied newly on November 2nd per Jose's instruction to match list of students with currently enrolled students. */ 
--INNER JOIN #CurrentlyEnrolledStudents s ON APC.StudentNumber = s.StudentNumber AND APC.EducationOrganizationId = s.EducationOrganizationID AND APC.ClassOf = s.ClassOf
;
GO


DECLARE @Year1 INT = 2022;
DECLARE @Year2 INT = 2023;
DECLARE @Year3 INT = 2024;
DECLARE @Year4 INT = 2025;
DROP TABLE #APIB_Cnts_UnmatchedToCurrentEnrolls;
SELECT 
	schoolType, EducationOrganizationID, 
	[Year1_AP], [Year1_IB], [Year1_DC], [Year1_CTE], [Year1_Overall], 	
	[Year2_AP], [Year2_IB], [Year2_DC], [Year2_CTE], [Year2_Overall], 
	[Year3_AP], [Year3_IB], [Year3_DC], [Year3_CTE], [Year3_Overall],
	[Year4_AP], [Year4_IB], [Year4_DC], [Year4_CTE], [Year4_Overall] 
INTO #APIB_Cnts_UnmatchedToCurrentEnrolls
FROM
( 
	SELECT schoolType, EducationOrganizationID, cntType, cnt FROM (
		SELECT schoolType, EducationOrganizationID, CONCAT('Year1', '_', 'AP') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE APIndicator = 1 AND ClassOf  = @Year1 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year2', '_', 'AP') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE APIndicator = 1 AND ClassOf  = @Year2 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year3', '_', 'AP') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE APIndicator = 1 AND ClassOf  = @Year3 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year4', '_', 'AP') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE APIndicator = 1 AND ClassOf  = @Year4 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year1', '_', 'IB') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE IBIndicator = 1 AND ClassOf  = @Year1 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year2', '_', 'IB') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE IBIndicator = 1 AND ClassOf  = @Year2 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year3', '_', 'IB') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE IBIndicator = 1 AND ClassOf  = @Year3 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year4', '_', 'IB') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE IBIndicator = 1 AND ClassOf  = @Year4 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year1', '_', 'DC') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE DualCreditIndicator = 1 AND ClassOf  = @Year1 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year2', '_', 'DC') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE DualCreditIndicator = 1 AND ClassOf  = @Year2 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year3', '_', 'DC') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE DualCreditIndicator = 1 AND ClassOf  = @Year3 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year4', '_', 'DC') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE DualCreditIndicator = 1 AND ClassOf  = @Year4 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year1', '_', 'CTE') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE CTEIndicator = 1 AND ClassOf  = @Year1 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year2', '_', 'CTE') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE CTEIndicator = 1 AND ClassOf  = @Year2 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year3', '_', 'CTE') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE CTEIndicator = 1 AND ClassOf  = @Year3 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year4', '_', 'CTE') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE CTEIndicator = 1 AND ClassOf  = @Year4 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year1', '_', 'Overall') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE APIndicator = 1 OR IBIndicator = 1 OR DualCreditIndicator = 1 OR CTEIndicator = 1 AND ClassOf  = @Year1 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year2', '_', 'Overall') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE APIndicator = 1 OR IBIndicator = 1 OR DualCreditIndicator = 1 OR CTEIndicator = 1 AND ClassOf  = @Year2 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year3', '_', 'Overall') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE APIndicator = 1 OR IBIndicator = 1 OR DualCreditIndicator = 1 OR CTEIndicator = 1 AND ClassOf  = @Year3 GROUP BY schoolType, EducationOrganizationID
		UNION
		SELECT schoolType, EducationOrganizationID, CONCAT('Year4', '_', 'Overall') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE APIndicator = 1 OR IBIndicator = 1 OR DualCreditIndicator = 1 OR CTEIndicator = 1 AND ClassOf  = @Year4 GROUP BY schoolType, EducationOrganizationID
	) L0
) mv
PIVOT ( MAX(cnt) FOR 
		cntType IN ( 
			[Year1_AP], [Year1_IB], [Year1_DC], [Year1_CTE], [Year1_Overall], 
			[Year2_AP], [Year2_IB], [Year2_DC], [Year2_CTE], [Year2_Overall],
			[Year3_AP], [Year3_IB], [Year3_DC], [Year3_CTE], [Year3_Overall],
			[Year4_AP], [Year4_IB], [Year4_DC], [Year4_CTE], [Year4_Overall] ) 
) pv
;

--SELECT * FROM #APIB_Cnts_UnmatchedToCurrentEnrolls
--SELECT * FROM #APIB_Cnts2

--SELECT schoolType, EducationOrganizationID AS schoolID FROM #APIB_Cnts
--EXCEPT
--SELECT schoolType, EducationOrganizationID AS schoolID FROM #APIB_Cnts_UnmatchedToCurrentEnrolls
--EXCEPT
--SELECT schoolType, EducationOrganizationID AS schoolID FROM #APIB_Cnts

--SELECT * FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE EducationOrganizationId = '844'
--SELECT * FROM SDM.dbo.DimEducationOrganization WHERE EducationOrganizationID = '844' AND EducationOrganizationCurrentRowIndicator = 'Current' 
--SELECT * FROM SDM.dbo.DimEducationOrganization WHERE CHARINDEX('adva', EducationOrganizationName) > 0  AND EducationOrganizationCurrentRowIndicator = 'Current' 

--SELECT * FROM (
--SELECT DISTINCT schoolType, EducationOrganizationID, studentNumber FROM #APIBDCCTE_UnmatchedToCurrentEnrolls 
--EXCEPT
--SELECT DISTINCT schoolType, EducationOrganizationID, studentNumber FROM #APIBDCCTE
--) KL WHERE EducationOrganizationID = '436' 

--SELECT * FROM #CohortsCnt  WHERE schoolID = '436'
--SELECT * FROM #APIB_Cnts WHERE EducationOrganizationID = '436' 
--SELECT * FROM #APIB_Cnts_UnmatchedToCurrentEnrolls WHERE EducationOrganizationID = '436' 

--SELECT e.* FROM #APIBDCCTE_UnmatchedToCurrentEnrolls e 
--INNER JOIN ( 
--	SELECT * FROM (
--		SELECT DISTINCT schoolType, EducationOrganizationID, studentNumber FROM #APIBDCCTE_UnmatchedToCurrentEnrolls 
--		EXCEPT
--		SELECT DISTINCT schoolType, EducationOrganizationID, studentNumber FROM #APIBDCCTE
--	) KL WHERE EducationOrganizationID = '436' 
--) L1 ON e.schoolType = L1.schoolType AND e.EducationOrganizationId = L1.EducationOrganizationId AND e.StudentNumber = L1.StudentNumber


DROP TABLE #APIB_List;
SELECT 
	a.schoolType, a.EducationOrganizationID AS schoolID , 
	CASE ISNULL(e.Year1, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year1_AP], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year1, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year1_AP_Perc],
	CASE ISNULL(e.Year1, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year1_IB], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year1, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year1_IB_Perc],
	CASE ISNULL(e.Year1, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year1_DC], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year1, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year1_DC_Perc],
	CASE ISNULL(e.Year1, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year1_CTE], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year1, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year1_CTE_Perc],
	CASE ISNULL(e.Year1, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year1_Overall], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year1, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year1_Overall_Perc],
	CASE ISNULL(e.Year2, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year2_AP], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year2, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year2_AP_Perc],
	CASE ISNULL(e.Year2, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year2_IB], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year2, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year2_IB_Perc],
	CASE ISNULL(e.Year2, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year2_DC], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year2, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year2_DC_Perc],
	CASE ISNULL(e.Year2, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year2_CTE], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year2, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year2_CTE_Perc],
	CASE ISNULL(e.Year2, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year2_Overall], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year2, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year2_Overall_Perc],
	CASE ISNULL(e.Year3, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year3_AP], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year3, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year3_AP_Perc],
	CASE ISNULL(e.Year3, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year3_IB], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year3, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year3_IB_Perc],
	CASE ISNULL(e.Year3, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year3_DC], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year3, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year3_DC_Perc],
	CASE ISNULL(e.Year3, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year3_CTE], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year3, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year3_CTE_Perc],
	CASE ISNULL(e.Year3, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year3_Overall], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year3, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year3_Overall_Perc],
	CASE ISNULL(e.Year4, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year4_AP], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year4, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year4_AP_Perc],
	CASE ISNULL(e.Year4, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year4_IB], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year4, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year4_IB_Perc],
	CASE ISNULL(e.Year4, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year4_DC], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year4, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year4_DC_Perc],
	CASE ISNULL(e.Year4, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year4_CTE], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year4, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year4_CTE_Perc],
	CASE ISNULL(e.Year4, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year4_Overall], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year4, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year4_Overall_Perc]
INTO #APIB_List
FROM #APIB_Cnts a 
INNER JOIN #CohortsCnt e ON a.schoolType = e.schoolType AND a.EducationOrganizationID = e.SchoolId ;


DROP TABLE #APIB_List_UnmatchedToCurrentEnrolls;
SELECT 
	a.schoolType, a.EducationOrganizationID AS schoolID , 
	CASE ISNULL(e.Year1, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year1_AP], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year1, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year1_AP_Perc],
	CASE ISNULL(e.Year1, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year1_IB], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year1, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year1_IB_Perc],
	CASE ISNULL(e.Year1, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year1_DC], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year1, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year1_DC_Perc],
	CASE ISNULL(e.Year1, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year1_CTE], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year1, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year1_CTE_Perc],
	CASE ISNULL(e.Year1, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year1_Overall], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year1, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year1_Overall_Perc],
	CASE ISNULL(e.Year2, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year2_AP], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year2, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year2_AP_Perc],
	CASE ISNULL(e.Year2, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year2_IB], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year2, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year2_IB_Perc],
	CASE ISNULL(e.Year2, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year2_DC], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year2, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year2_DC_Perc],
	CASE ISNULL(e.Year2, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year2_CTE], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year2, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year2_CTE_Perc],
	CASE ISNULL(e.Year2, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year2_Overall], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year2, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year2_Overall_Perc],
	CASE ISNULL(e.Year3, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year3_AP], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year3, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year3_AP_Perc],
	CASE ISNULL(e.Year3, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year3_IB], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year3, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year3_IB_Perc],
	CASE ISNULL(e.Year3, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year3_DC], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year3, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year3_DC_Perc],
	CASE ISNULL(e.Year3, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year3_CTE], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year3, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year3_CTE_Perc],
	CASE ISNULL(e.Year3, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year3_Overall], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year3, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year3_Overall_Perc],
	CASE ISNULL(e.Year4, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year4_AP], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year4, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year4_AP_Perc],
	CASE ISNULL(e.Year4, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year4_IB], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year4, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year4_IB_Perc],
	CASE ISNULL(e.Year4, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year4_DC], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year4, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year4_DC_Perc],
	CASE ISNULL(e.Year4, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year4_CTE], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year4, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year4_CTE_Perc],
	CASE ISNULL(e.Year4, 0) WHEN 0 THEN NULL ELSE ROUND(CAST(ISNULL([Year4_Overall], 0) AS DECIMAL(10, 4)) / CAST(ISNULL(e.Year4, 0) AS DECIMAL(10, 4)), 4, 1) END AS [Year4_Overall_Perc]
INTO #APIB_List_UnmatchedToCurrentEnrolls
FROM #APIB_Cnts_UnmatchedToCurrentEnrolls a 
INNER JOIN #CohortsCnt_UnmatchedToCurrentEnrolls e ON a.schoolType = e.schoolType AND a.EducationOrganizationID = e.SchoolId ;

SELECT 
	a0.schoolType, 
	a0.schoolID, 
	a0.Year1_CTE_Perc AS orig_Year1CTE, 
	a1.Year1_CTE_Perc AS new_Year1CTE, 
	a0.Year1_CTE_Perc - a1.Year1_CTE_Perc AS Diff_Year1,
	a0.Year2_CTE_Perc AS orig_Year2CTE, 
	a1.Year2_CTE_Perc AS new_Year2CTE, 
	a0.Year2_CTE_Perc - a1.Year2_CTE_Perc AS Diff_Year2,
	a0.Year3_CTE_Perc AS orig_Year3CTE, 
	a1.Year3_CTE_Perc AS new_Year3CTE, 
	a0.Year3_CTE_Perc - a1.Year3_CTE_Perc AS Diff_Year3,
	a0.Year4_CTE_Perc AS orig_Year4CTE, 
	a1.Year4_CTE_Perc AS new_Year4CTE, 
	a0.Year4_CTE_Perc - a1.Year4_CTE_Perc AS Diff_Year4
FROM #APIB_List_UnmatchedToCurrentEnrolls a0 INNER JOIN 
(
	SELECT * FROM #APIB_List
	EXCEPT
	SELECT * FROM #APIB_List_UnmatchedToCurrentEnrolls
) a1 ON a0.schoolType = a1.schoolType AND a0.schoolID = a1.schoolID 
ORDER BY Diff_Year1 DESC 


SELECT schoolType, EducationOrganizationID, CONCAT('Year1', '_', 'CTE') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE CTEIndicator = 1 AND ClassOf  = @Year1 GROUP BY schoolType, EducationOrganizationID
UNION
SELECT schoolType, EducationOrganizationID, CONCAT('Year2', '_', 'CTE') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE CTEIndicator = 1 AND ClassOf  = @Year2 GROUP BY schoolType, EducationOrganizationID
UNION
SELECT schoolType, EducationOrganizationID, CONCAT('Year3', '_', 'CTE') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE CTEIndicator = 1 AND ClassOf  = @Year3 GROUP BY schoolType, EducationOrganizationID
UNION
SELECT schoolType, EducationOrganizationID, CONCAT('Year4', '_', 'CTE') AS cntType, COUNT(DISTINCT StudentNumber) cnt FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE CTEIndicator = 1 AND ClassOf  = '2025' AND EducationOrganizationId = '550' GROUP BY schoolType, EducationOrganizationID


SELECT * FROM SDM.dbo.DimEducationOrganization WHERE EducationOrganizationID = '942' AND EducationOrganizationCurrentRowIndicator = 'Current' 

SELECT schoolType, EducationOrganizationID, CONCAT('Year4', '_', 'CTE') AS cntType, COUNT(DISTINCT StudentNumber) cnt 
FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE CTEIndicator = 1 AND ClassOf  = '2025' AND EducationOrganizationID = '550' GROUP BY schoolType, EducationOrganizationID

SELECT *
FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE CTEIndicator = 1 AND ClassOf  = '2025' AND EducationOrganizationID = '550'
SELECT *
FROM #APIBDCCTE  WHERE CTEIndicator = 1 AND ClassOf  = '2025' AND EducationOrganizationID = '550'

SELECT * FROM #APIB_Cnts_UnmatchedToCurrentEnrolls WHERE EducationOrganizationId = '550'
SELECT * FROM #APIB_Cnts WHERE EducationOrganizationId = '550'

SELECT DISTINCT studentNumber FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE EducationOrganizationId = '550' AND ClassOf = '2022' --AND GradeLevelCode = '09'
SELECT * FROM #APIBDCCTE WHERE EducationOrganizationId = '550' AND ClassOf = '2022' -- AND GradeLevelCode = '09'


SELECT DISTINCT studentNumber FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE EducationOrganizationId = '550' AND ClassOf = '2022' --AND GradeLevelCode = '09'
EXCEPT 
SELECT DISTINCT studentNumber FROM #APIBDCCTE WHERE EducationOrganizationId = '550' AND ClassOf = '2022' -- AND GradeLevelCode = '09'
EXCEPT 
SELECT DISTINCT studentNumber FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE EducationOrganizationId = '550' AND ClassOf = '2022' --AND GradeLevelCode = '09'

SELECT * FROM #APIBDCCTE WHERE studentNumber = '1179888'


SELECT * FROM #APIB_Cnts_UnmatchedToCurrentEnrolls WHERE EducationOrganizationId = '846'
SELECT * FROM #APIB_Cnts WHERE EducationOrganizationId = '846'

SELECT * FROM #APIBDCCTE_UnmatchedToCurrentEnrolls WHERE EducationOrganizationId = '846' AND ClassOf = '2022' --AND GradeLevelCode = '09'
SELECT * FROM #APIBDCCTE WHERE EducationOrganizationId = '846' AND ClassOf = '2022' -- AND GradeLevelCode = '09'



DECLARE @DT DATE = CAST(GETDATE() AS DATE);
DROP TABLE #CAs_ByGroup;
SELECT
	CCSDLoc, SchoolType, 
	groupType, 
	sum( iif( status = 'chronic', 1, 0 ) ) as numerator, 
	sum( iif( caEligibility in ('valid','possible'), 1, 0 ) ) as denominator,
	sum( iif( status = 'chronic', 1, 0 ) ) * 1.0 / iif( sum( iif( caEligibility in ('valid','possible'), 1, 0 ) ) = 0, null, sum( iif( caEligibility in ('valid','possible'), 1, 0 ) ) ) as CA_Perc
INTO #CAs_ByGroup
FROM ( 
	SELECT DISTINCT 
		school, 
		CCSDLoc, 
		CASE 
			WHEN grade IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' 
			WHEN grade IN ('06', '07', '08' ) THEN 'MS' 
			WHEN grade IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' 
		END SchoolType,
		[status], 		
		[student #], 
		'Overall' AS groupType,
		CAEligibility
	FROM [ORION.CIS.CCSD.NET].[SDM].[ReportingDatasets].[ChronicAbsenteeism] CA
	WHERE 
		schoolYear = 2022
		and CAEligibility in ('valid','possible')
	UNION
	SELECT DISTINCT 
		school, 
		CCSDLoc, 
		CASE 
			WHEN grade IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' 
			WHEN grade IN ('06', '07', '08' ) THEN 'MS' 
			WHEN grade IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' 
		END SchoolType,
		[status], 		
		[student #], 
		[StudentEthnicityCode] AS groupType,
		CAEligibility
	FROM [ORION.CIS.CCSD.NET].[SDM].[ReportingDatasets].[ChronicAbsenteeism] CA
	WHERE 
		schoolYear = 2022
		and CAEligibility in ('valid','possible')
	UNION
	SELECT DISTINCT 
		school, 
		CCSDLoc, 
		CASE 
			WHEN grade IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' 
			WHEN grade IN ('06', '07', '08' ) THEN 'MS' 
			WHEN grade IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' 
		END SchoolType,
		[status], 		
		[student #], 
		CASE grade WHEN 'TP' THEN 'PK' ELSE grade END AS groupType,
		CAEligibility
	FROM [ORION.CIS.CCSD.NET].[SDM].[ReportingDatasets].[ChronicAbsenteeism] CA
	WHERE 
		schoolYear = 2022
		and CAEligibility in ('valid','possible')
	UNION
	SELECT DISTINCT 
		school, 
		CCSDLoc, 
		CASE 
			WHEN grade IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' 
			WHEN grade IN ('06', '07', '08' ) THEN 'MS' 
			WHEN grade IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' 
		END SchoolType,
		[status], 		
		[student #], 
		'IEP' AS groupType,
		CAEligibility
	FROM [ORION.CIS.CCSD.NET].[SDM].[ReportingDatasets].[ChronicAbsenteeism] CA
	WHERE schoolYear = 2022
	and CAEligibility in ('valid','possible')
	AND IEPIndicator IN ( 'IEP', 'Y' ) 
	UNION
	SELECT DISTINCT 
		school, 
		CCSDLoc, 
		CASE 
			WHEN grade IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' 
			WHEN grade IN ('06', '07', '08' ) THEN 'MS' 
			WHEN grade IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' 
		END SchoolType,
		[status], 		
		[student #], 
		'LEP' AS groupType,
		CAEligibility
	FROM [ORION.CIS.CCSD.NET].[SDM].[ReportingDatasets].[ChronicAbsenteeism] CA
	WHERE schoolYear = 2022
	and CAEligibility in ('valid','possible')
	AND LEPIndicator IN ( 'LEP', 'Y' ) 
	UNION
	SELECT 
		school, 
		CCSDLoc, 
		SchoolType,
		[status], 		
		[student #], 		
		'FRL' AS groupType,
		CAEligibility
	FROM ( 
		SELECT DISTINCT 
			CA.school, 
			CA.CCSDLoc, 
			CASE 
				WHEN CA.grade IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' 
				WHEN CA.grade IN ('06', '07', '08' ) THEN 'MS' 
				WHEN CA.grade IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' 
			END SchoolType,
			CA.[status], 		
			CA.[student #], 					
			CA.CAEligibility, 
			IIF(fs.[Local School Number] IS NULL, 0, 1) + IIF(f.[StudentNumber] IS NULL, 0, 1) AS IsFRL
		FROM [ORION.CIS.CCSD.NET].[SDM].[ReportingDatasets].[ChronicAbsenteeism] CA
		LEFT JOIN [FRL].dbo.[FRL] f ON CA.[Student #] = f.StudentNumber		
			AND f.CurrentRowIndicator = 'Current'
			AND f.RowEffectiveDate <= @DT
			AND f.Eligibility IN ( 'F', 'R' )
		LEFT JOIN #FRLschoolsList fs ON CA.CCSDLOC = fs.[Local School Number] 		
		WHERE 
			CA.schoolYear = 2022
			and CA.CAEligibility in ('valid','possible')
	) L1 
	WHERE IsFRL > 0 		
) L1 
group by CCSDLOC, SchoolType, groupType
;

DROP TABLE #CA_List;
SELECT 
	CCSDLoc, SchoolType, 
	ISNULL([Overall], 0) AS [Overall_CA_Perc], 
	ISNULL([PK], 0) AS [PK_CA_Perc], ISNULL([0K], 0) AS [0K_CA_Perc], ISNULL([01], 0) AS [01_CA_Perc], ISNULL([02], 0) AS [02_CA_Perc], ISNULL([03], 0) AS [03_CA_Perc], ISNULL([04], 0) AS [04_CA_Perc], ISNULL([05], 0) AS [05_CA_Perc], 
	ISNULL([06], 0) AS [06_CA_Perc], ISNULL([07], 0) AS [07_CA_Perc], ISNULL([08], 0) AS [08_CA_Perc], 
	ISNULL([09], 0) AS [09_CA_Perc], ISNULL([10], 0) AS [10_CA_Perc], ISNULL([11], 0) AS [11_CA_Perc], ISNULL([12], 0) AS [12_CA_Perc], ISNULL([AD], 0) AS [AD_CA_Perc], ISNULL([UN], 0) AS [UN_CA_Perc], 
	ISNULL([A], 0) AS [Asian_CA_Perc], ISNULL([B], 0) AS [Black_CA_Perc], ISNULL([C], 0) AS [Cauca_CA_Perc], ISNULL([H], 0) AS [Hispanics_CA_Perc], 
	ISNULL([I], 0) AS [NatAmerican_CA_Perc], ISNULL([M], 0) AS [MultiRacial_CA_Perc], ISNULL([P], 0) AS [PacIslanders_CA_Perc], 
	ISNULL([IEP], 0) AS [IEP_CA_Perc], ISNULL([LEP], 0) AS [EL_CA_Perc], ISNULL([FRL], 0) AS [FRL_CA_Perc]	
INTO #CA_List
FROM 
( SELECT CCSDLoc, SchoolType, groupType, CA_Perc FROM #CAs_ByGroup ) mv 
PIVOT ( MAX(CA_Perc) FOR  groupType IN ( [Overall], [PK], [0K], [01], [02], [03], [04], [05], [06], [07], [08], [09], [10], [11], [12], [AD], [UN], [A], [B], [C], [H], [I], [M], [P], [IEP], [LEP], [FRL] ) ) pv
;



DROP TABLE dbo.QuarterlyProgressMonitoring
;
DECLARE @Year1 INT = 2022;
DECLARE @Year2 INT = 2023;
DECLARE @Year3 INT = 2024;
DECLARE @Year4 INT = 2025;
DECLARE @DTQtrStart DATE = CAST('2021-08-09' AS DATE);
DECLARE @DTQtrEnd DATE = CAST('2021-10-08' AS DATE);
DECLARE @CurrentTerm VARCHAR(10) = 'Q1';

SELECT 
	@DTQtrStart = CAST(QuarterStartDate AS DATE), @DTQtrEnd = CAST(QuarterEndDate AS DATE) 
FROM #DatesList WHERE Term = @CurrentQtr;


DROP TABLE #resultsCombined;
-- Also add region, school name, trustee zone, 
SELECT DISTINCT 
	2022 AS schoolYear, 
	'Q1' AS [Quarter],
	CAST(GETDATE() AS DATE) AS [Snapshot_Date],
	@DTQtrEnd AS [Qtr End Date],
	edl.AcademicOrganizationalUnitCode, 
	edl.TrusteeDistrict,
	edl.TrusteeNameFull,
	sL.schoolType, 
	sL.schoolID AS [CCSD#], 
	edL.EducationOrganizationShortName,		
	ISNULL(e.[A], 0) AS [Asian_Enrolled], ISNULL(e.[B], 0) AS [Black_Enrolled], ISNULL(e.[C], 0) AS [Cauca_Enrolled], ISNULL(e.[H], 0) AS [Hispanics_Enrolled], ISNULL(e.[I], 0) AS [NatAmerican_Enrolled], ISNULL(e.[M], 0) AS [MultiRacial_Enrolled], ISNULL(e.[P], 0) AS [PacIslanders_Enrolled], 
	ISNULL(e.[IEP], 0) AS [IEP_Enrolled], ISNULL(e.[EL], 0) AS [EL_Enrolled], ISNULL(e.[FRL], 0) AS [FRL_Enrolled], 
	ISNULL(e.[NewlyEnrolled], 0) AS [NewlyEnrolled], ISNULL(e.[NewlyExited], 0) AS [NewlyExited], 
	--[PK_AttdPerc], [0K_AttdPerc], [01_AttdPerc], [02_AttdPerc], [03_AttdPerc], [04_AttdPerc], [05_AttdPerc], [06_AttdPerc], [07_AttdPerc], [08_AttdPerc], [09_AttdPerc], [10_AttdPerc], [11_AttdPerc], [12_AttdPerc], [AD_AttdPerc], [UN_AttdPerc],  
	--a.AbsCntQt1, a.[AbsCntYTD], 	
	ISNULL(bq.cnt, 0) AS BehaviorQtr, ISNULL(bytd.cnt, 0) AS BehaviorYTD,
	ISNULL(btype.[# of Major Incident], 0) AS [# of Major Incident In Qtr], 
	ISNULL(btype.[# of Minor Incident], 0) AS [# of Minor Incident In Qtr], 
	ISNULL(bsc.cnt, 0) AS [# of Students Removed From Instruction In Qtr], 
	ISNULL(ca.[Overall_CA_Perc], 0) AS [Overall_CA_Perc], 
	ISNULL(ca.[Asian_CA_Perc], 0) AS [Asian_CA_Perc], ISNULL(ca.[Black_CA_Perc], 0) AS [Black_CA_Perc], ISNULL(ca.[Cauca_CA_Perc], 0) AS [Cauca_CA_Perc], 
	ISNULL(ca.[Hispanics_CA_Perc], 0) AS [Hispanics_CA_Perc], ISNULL(ca.[NatAmerican_CA_Perc], 0) AS [NatAmerican_CA_Perc], ISNULL(ca.[MultiRacial_CA_Perc], 0) AS [MultiRacial_CA_Perc], 
	ISNULL(ca.[PacIslanders_CA_Perc], 0) AS [PacIslanders_CA_Perc], ISNULL(ca.[IEP_CA_Perc], 0) AS [IEP_CA_Perc], ISNULL(ca.[EL_CA_Perc], 0) AS [EL_CA_Perc], ISNULL(ca.[FRL_CA_Perc], 0) AS [FRL_CA_Perc], 
	ISNULL(ca.[0K_CA_Perc], 0) AS [0K_CA_Perc], ISNULL(ca.[01_CA_Perc], 0) AS [01_CA_Perc], ISNULL(ca.[02_CA_Perc], 0) AS [02_CA_Perc], ISNULL(ca.[03_CA_Perc], 0) AS [03_CA_Perc], ISNULL(ca.[04_CA_Perc], 0) AS [04_CA_Perc], 
	ISNULL(ca.[05_CA_Perc], 0) AS [05_CA_Perc], ISNULL(ca.[06_CA_Perc], 0) AS [06_CA_Perc], ISNULL(ca.[07_CA_Perc], 0) AS [07_CA_Perc], ISNULL(ca.[08_CA_Perc], 0) AS [08_CA_Perc], ISNULL(ca.[09_CA_Perc], 0) AS [09_CA_Perc], 
	ISNULL(ca.[10_CA_Perc], 0) AS [10_CA_Perc], ISNULL(ca.[11_CA_Perc], 0) AS [11_CA_Perc], ISNULL(ca.[12_CA_Perc], 0) AS [12_CA_Perc], 
	ISNULL(ml.[Fall Mathematics Growth Perc], 0) AS [Fall Mathematics Growth Perc], ISNULL(ml.[Fall Mathematics IsProficient Perc], 0) AS [Fall Mathematics IsProficient Perc], CAST(ISNULL(ml.[Fall Mathematics Partic Std #], 0) AS INT) AS [Fall Mathematics Partic Std #], [Fall Mathematics To40Perc], [Fall Mathematics 41To69Perc], [Fall Mathematics Above70Perc], FORMAT(ROUND([Fall Mathematics To40Cnt], 0, 1), 'N0') AS [Fall Mathematics To40Cnt], FORMAT(ROUND([Fall Mathematics 41To69Cnt], 0, 1), 'N0') AS [Fall Mathematics 41To69Cnt], FORMAT(ROUND([Fall Mathematics Above70Cnt], 0, 1), 'N0') AS [Fall Mathematics Above70Cnt], [Fall Mathematics ProjectedProfPerc], FORMAT(ROUND([Fall Mathematics ProjectedProfCnt], 0, 1), 'N0') AS [Fall Mathematics ProjectedProfCnt], FORMAT(ROUND([Fall Mathematics ProjectedProfPopulationCnt], 0, 1), 'N0') AS [Fall Mathematics ProjectedProfPopulationCnt],  
	ISNULL(ml.[Fall Reading Growth Perc], 0) AS [Fall Reading Growth Perc], ISNULL(ml.[Fall Reading IsProficient Perc], 0) AS [Fall Reading IsProficient Perc], CAST(ISNULL(ml.[Fall Reading Partic Std #], 0) AS INT) AS [Fall Reading Partic Std #], [Fall Reading To40Perc], [Fall Reading 41To69Perc], [Fall Reading Above70Perc], FORMAT(ROUND([Fall Reading To40Cnt], 0, 1), 'N0') AS [Fall Reading To40Cnt], FORMAT(ROUND([Fall Reading 41To69Cnt], 0, 1), 'N0') AS [Fall Reading 41To69Cnt], FORMAT(ROUND([Fall Reading Above70Cnt], 0, 1), 'N0') AS [Fall Reading Above70Cnt], [Fall Reading ProjectedProfPerc], FORMAT(ROUND([Fall Reading ProjectedProfCnt], 0, 1), 'N0') AS [Fall Reading ProjectedProfCnt], FORMAT(ROUND([Fall Reading ProjectedProfPopulationCnt], 0, 1), 'N0') AS [Fall Reading ProjectedProfPopulationCnt],
	ISNULL(ml.[Fall Science Growth Perc], 0) AS [Fall Science Growth Perc], ISNULL(ml.[Fall Science IsProficient Perc], 0) AS [Fall Science IsProficient Perc], CAST(ISNULL(ml.[Fall Science Partic Std #], 0) AS INT) AS [Fall Science Partic Std #], [Fall Science To40Perc], [Fall Science 41To69Perc], [Fall Science Above70Perc], FORMAT(ROUND([Fall Science To40Cnt], 0, 1), 'N0') AS [Fall Science To40Cnt], FORMAT(ROUND([Fall Science 41To69Cnt], 0, 1), 'N0') AS [Fall Science 41To69Cnt], FORMAT(ROUND([Fall Science Above70Cnt], 0, 1), 'N0') AS [Fall Science Above70Cnt], [Fall Science ProjectedProfPerc], FORMAT(ROUND([Fall Science ProjectedProfCnt], 0, 1), 'N0') AS [Fall Science ProjectedProfCnt], FORMAT(ROUND([Fall Science ProjectedProfPopulationCnt], 0, 1), 'N0') AS [Fall Science ProjectedProfPopulationCnt],		
	ISNULL(ml.[Winter Mathematics Growth Perc], 0) AS [Winter Mathematics Growth Perc], ISNULL(ml.[Winter Mathematics IsProficient Perc], 0) AS [Winter Mathematics IsProficient Perc], CAST(ISNULL(ml.[Winter Mathematics Partic Std #], 0) AS INT) AS [Winter Mathematics Partic Std #], [Winter Mathematics To40Perc], [Winter Mathematics 41To69Perc], [Winter Mathematics Above70Perc], FORMAT(ROUND([Winter Mathematics To40Cnt], 0, 1), 'N0') AS [Winter Mathematics To40Cnt], FORMAT(ROUND([Winter Mathematics 41To69Cnt], 0, 1), 'N0') AS [Winter Mathematics 41To69Cnt], FORMAT(ROUND([Winter Mathematics Above70Cnt], 0, 1), 'N0') AS [Winter Mathematics Above70Cnt], [Winter Mathematics ProjectedProfPerc], FORMAT(ROUND([Winter Mathematics ProjectedProfCnt], 0, 1), 'N0') AS [Winter Mathematics ProjectedProfCnt], FORMAT(ROUND([Winter Mathematics ProjectedProfPopulationCnt], 0, 1), 'N0') AS [Winter Mathematics ProjectedProfPopulationCnt],  
	ISNULL(ml.[Winter Reading Growth Perc], 0) AS [Winter Reading Growth Perc], ISNULL(ml.[Winter Reading IsProficient Perc], 0) AS [Winter Reading IsProficient Perc], CAST(ISNULL(ml.[Winter Reading Partic Std #], 0) AS INT) AS [Winter Reading Partic Std #], [Winter Reading To40Perc], [Winter Reading 41To69Perc], [Winter Reading Above70Perc], FORMAT(ROUND([Winter Reading To40Cnt], 0, 1), 'N0') AS [Winter Reading To40Cnt], FORMAT(ROUND([Winter Reading 41To69Cnt], 0, 1), 'N0') AS [Winter Reading 41To69Cnt], FORMAT(ROUND([Winter Reading Above70Cnt], 0, 1), 'N0') AS [Winter Reading Above70Cnt], [Winter Reading ProjectedProfPerc], FORMAT(ROUND([Winter Reading ProjectedProfCnt], 0, 1), 'N0') AS [Winter Reading ProjectedProfCnt], FORMAT(ROUND([Winter Reading ProjectedProfPopulationCnt], 0, 1), 'N0') AS [Winter Reading ProjectedProfPopulationCnt],  
	ISNULL(ml.[Winter Science Growth Perc], 0) AS [Winter Science Growth Perc], ISNULL(ml.[Winter Science IsProficient Perc], 0) AS [Winter Science IsProficient Perc], CAST(ISNULL(ml.[Winter Science Partic Std #], 0) AS INT) AS [Winter Science Partic Std #], [Winter Science To40Perc], [Winter Science 41To69Perc], [Winter Science Above70Perc], FORMAT(ROUND([Winter Science To40Cnt], 0, 1), 'N0') AS [Winter Science To40Cnt], FORMAT(ROUND([Winter Science 41To69Cnt], 0, 1), 'N0') AS [Winter Science 41To69Cnt], FORMAT(ROUND([Winter Science Above70Cnt], 0, 1), 'N0') AS [Winter Science Above70Cnt], [Winter Science ProjectedProfPerc], FORMAT(ROUND([Winter Science ProjectedProfCnt], 0, 1), 'N0') AS [Winter Science ProjectedProfCnt], FORMAT(ROUND([Winter Science ProjectedProfPopulationCnt], 0, 1), 'N0') AS [Winter Science ProjectedProfPopulationCnt],  
	ISNULL(ml.[Spring Mathematics Growth Perc], 0) AS [Spring Mathematics Growth Perc], ISNULL(ml.[Spring Mathematics IsProficient Perc], 0) AS [Spring Mathematics IsProficient Perc], CAST(ISNULL(ml.[Spring Mathematics Partic Std #], 0) AS INT) AS [Spring Mathematics Partic Std #], [Spring Mathematics To40Perc], [Spring Mathematics 41To69Perc], [Spring Mathematics Above70Perc], FORMAT(ROUND([Spring Mathematics To40Cnt], 0, 1), 'N0') AS [Spring Mathematics To40Cnt], FORMAT(ROUND([Spring Mathematics 41To69Cnt], 0, 1), 'N0') AS [Spring Mathematics 41To69Cnt], FORMAT(ROUND([Spring Mathematics Above70Cnt], 0, 1), 'N0') AS [Spring Mathematics Above70Cnt], [Spring Mathematics ProjectedProfPerc], FORMAT(ROUND([Spring Mathematics ProjectedProfCnt], 0, 1), 'N0') AS [Spring Mathematics ProjectedProfCnt], FORMAT(ROUND([Spring Mathematics ProjectedProfPopulationCnt], 0, 1), 'N0') AS [Spring Mathematics ProjectedProfPopulationCnt],  
	ISNULL(ml.[Spring Reading Growth Perc], 0) AS [Spring Reading Growth Perc], ISNULL(ml.[Spring Reading IsProficient Perc], 0) AS [Spring Reading IsProficient Perc], CAST(ISNULL(ml.[Spring Reading Partic Std #], 0) AS INT) AS [Spring Reading Partic Std #], [Spring Reading To40Perc], [Spring Reading 41To69Perc], [Spring Reading Above70Perc], FORMAT(ROUND([Spring Reading To40Cnt], 0, 1), 'N0') AS [Spring Reading To40Cnt], FORMAT(ROUND([Spring Reading 41To69Cnt], 0, 1), 'N0') AS [Spring Reading 41To69Cnt], FORMAT(ROUND([Spring Reading Above70Cnt], 0, 1), 'N0') AS [Spring Reading Above70Cnt], [Spring Reading ProjectedProfPerc], FORMAT(ROUND([Spring Reading ProjectedProfCnt], 0, 1), 'N0') AS [Spring Reading ProjectedProfCnt], FORMAT(ROUND([Spring Reading ProjectedProfPopulationCnt], 0, 1), 'N0') AS [Spring Reading ProjectedProfPopulationCnt],  
	ISNULL(ml.[Spring Science Growth Perc], 0) AS [Spring Science Growth Perc], ISNULL(ml.[Spring Science IsProficient Perc], 0) AS [Spring Science IsProficient Perc], CAST(ISNULL(ml.[Spring Science Partic Std #], 0) AS INT) AS [Spring Science Partic Std #], [Spring Science To40Perc], [Spring Science 41To69Perc], [Spring Science Above70Perc], FORMAT(ROUND([Spring Science To40Cnt], 0, 1), 'N0') AS [Spring Science To40Cnt], FORMAT(ROUND([Spring Science 41To69Cnt], 0, 1), 'N0') AS [Spring Science 41To69Cnt], FORMAT(ROUND([Spring Science Above70Cnt], 0, 1), 'N0') AS [Spring Science Above70Cnt], [Spring Science ProjectedProfPerc], FORMAT(ROUND([Spring Science ProjectedProfCnt], 0, 1), 'N0') AS [Spring Science ProjectedProfCnt], FORMAT(ROUND([Spring Science ProjectedProfPopulationCnt], 0, 1), 'N0') AS [Spring Science ProjectedProfPopulationCnt],  
	ISNULL(g.[Qtr_letterACnt], 0) AS [Qtr_letterACnt], ISNULL(g.[Qtr_letterBCnt], 0) AS [Qtr_letterBCnt], ISNULL(g.[Qtr_letterCCnt], 0) AS [Qtr_letterCCnt], ISNULL(g.[Qtr_letterDCnt], 0) AS [Qtr_letterDCnt], ISNULL(g.[Qtr_letterFCnt], 0) AS [Qtr_letterFCnt], 
	ISNULL(g.[Qtr_letterECnt], 0) AS [Qtr_letterECnt], ISNULL(g.[Qtr_letterSCnt], 0) AS [Qtr_letterSCnt], ISNULL(g.[Qtr_letterNCnt], 0) AS [Qtr_letterNCnt], 
	ISNULL(g.[Qtr_letter1Cnt], 0) AS [Qtr_letter1Cnt], ISNULL(g.[Qtr_letter2Cnt], 0) AS [Qtr_letter2Cnt], ISNULL(g.[Qtr_letter3Cnt], 0) AS [Qtr_letter3Cnt], ISNULL(g.[Qtr_letter4Cnt], 0) AS [Qtr_letter4Cnt], 
	ISNULL(g.[Semester_letterACnt], 0) AS [Semester_letterACnt], ISNULL(g.[Semester_letterBCnt], 0) AS [Semester_letterBCnt], ISNULL(g.[Semester_letterCCnt], 0) AS [Semester_letterCCnt], ISNULL(g.[Semester_letterDCnt], 0) AS [Semester_letterDCnt], ISNULL(g.[Semester_letterFCnt], 0) AS [Semester_letterFCnt], 
	ISNULL(g.[Semester_letterECnt], 0) AS [Semester_letterECnt], ISNULL(g.[Semester_letterSCnt], 0) AS [Semester_letterSCnt], ISNULL(g.[Semester_letterNCnt], 0) AS [Semester_letterNCnt], 
	ISNULL(g.[Semester_letter1Cnt], 0) AS [Semester_letter1Cnt], ISNULL(g.[Semester_letter2Cnt], 0) AS [Semester_letter2Cnt], ISNULL(g.[Semester_letter3Cnt], 0) AS [Semester_letter3Cnt], ISNULL(g.[Semester_letter4Cnt], 0) AS [Semester_letter4Cnt], 
	@Year1 AS Year1_Year, ISNULL(t.[Year1_OnTrack], 0) AS [Year1_OnTrack], ISNULL([Year1_AP_Perc], 0) AS [Year1_AP_Perc], ISNULL([Year1_IB_Perc], 0) AS [Year1_IB_Perc], ISNULL([Year1_DC_Perc], 0) AS [Year1_DC_Perc], ISNULL([Year1_CTE_Perc], 0) AS [Year1_CTE_Perc], ISNULL([Year1_Overall_Perc], 0) AS [Year1_Overall_Perc], 
	@Year2 AS Year2_Year, ISNULL(t.[Year2_OnTrack], 0) AS [Year2_OnTrack], ISNULL([Year2_AP_Perc], 0) AS [Year2_AP_Perc], ISNULL([Year2_IB_Perc], 0) AS [Year2_IB_Perc], ISNULL([Year2_DC_Perc], 0) AS [Year2_DC_Perc], ISNULL([Year2_CTE_Perc], 0) AS [Year2_CTE_Perc], ISNULL([Year2_Overall_Perc], 0) AS [Year2_Overall_Perc], 
	@Year3 AS Year3_Year, ISNULL(t.[Year3_OnTrack], 0) AS [Year3_OnTrack], ISNULL([Year3_AP_Perc], 0) AS [Year3_AP_Perc], ISNULL([Year3_IB_Perc], 0) AS [Year3_IB_Perc], ISNULL([Year3_DC_Perc], 0) AS [Year3_DC_Perc], ISNULL([Year3_CTE_Perc], 0) AS [Year3_CTE_Perc], ISNULL([Year3_Overall_Perc], 0) AS [Year3_Overall_Perc], 
	@Year4 AS Year4_Year, ISNULL(t.[Year4_OnTrack], 0) AS [Year4_OnTrack], ISNULL([Year4_AP_Perc], 0) AS [Year4_AP_Perc], ISNULL([Year4_IB_Perc], 0) AS [Year4_IB_Perc], ISNULL([Year4_DC_Perc], 0) AS [Year4_DC_Perc], ISNULL([Year4_CTE_Perc], 0) AS [Year4_CTE_Perc], ISNULL([Year4_Overall_Perc], 0) AS [Year4_Overall_Perc]
	--[2023_AP_Perc], [2023_IB_Perc], [2023_DC_Perc], [2023_CTE_Perc], [2023_Overall_Perc], [2024_AP_Perc], [2024_IB_Perc], [2024_DC_Perc], [2024_CTE_Perc], [2024_Overall_Perc], [2025_AP_Perc], [2025_IB_Perc], [2025_DC_Perc], [2025_CTE_Perc], [2025_Overall_Perc]
INTO dbo.QuarterlyProgressMonitoring
--INTO #resultsCombined
FROM 
(
	SELECT DISTINCT schoolType, LTRIM(RTRIM(CAST(schoolID AS VARCHAR(10)))) AS schoolID FROM #AttendanceList	
	UNION
	SELECT DISTINCT schoolType, LTRIM(RTRIM(CAST(EducationOrganizationID AS VARCHAR(10)))) AS schoolID FROM #Enrollment 
	UNION 
	SELECT DISTINCT schoolType, LTRIM(RTRIM(CAST(schoolID AS VARCHAR(10)))) AS schoolID FROM #OnTrackList
	UNION 
	SELECT DISTINCT schoolType, LTRIM(RTRIM(CAST(schoolID AS VARCHAR(10)))) AS schoolID FROM #MAPsList
	--SELECT DISTINCT schoolType, LTRIM(RTRIM(CAST(schoolID AS VARCHAR(10)))) AS schoolID FROM  #MAPsGrowthList
	--UNION 
	--SELECT DISTINCT schoolType, LTRIM(RTRIM(CAST(schoolID AS VARCHAR(10)))) AS schoolID FROM  #MAPsproficientList
	UNION 
	SELECT DISTINCT schoolType, LTRIM(RTRIM(CAST(schoolID AS VARCHAR(10)))) AS schoolID FROM  #GradeDistList	
	UNION 
	SELECT DISTINCT schoolType, LTRIM(RTRIM(CAST(CCSDLoc AS VARCHAR(10)))) AS schoolID FROM  #CA_List
	UNION
	SELECT DISTINCT schoolType, LTRIM(RTRIM(CAST( EducationOrganizationID AS VARCHAR(10)))) AS schoolID FROM  #Enrollment
	UNION 
	SELECT DISTINCT schoolType, LTRIM(RTRIM(CAST( schoolID AS VARCHAR(10)))) AS schoolID FROM #APIB_List 
) sL 
INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimEducationOrganization edl ON sL.schoolID = edl.EducationOrganizationID 
LEFT JOIN #Enrollment e ON LTRIM(RTRIM(CAST( e.EducationOrganizationID AS VARCHAR(10)))) = sL.schoolID AND e.schoolType = sL.schoolType
--LEFT JOIN #AttendanceList AS a ON LTRIM(RTRIM(CAST( a.schoolID AS VARCHAR(10)))) = sL.schoolID AND a.schoolType = sL.schoolType 
LEFT JOIN #BehaviorQtr AS bq ON LTRIM(RTRIM(CAST( bq.schoolID AS VARCHAR(10)))) = sL.schoolID AND bq.schoolType = sL.schoolType 
LEFT JOIN #BehaviorYTD AS bytd ON LTRIM(RTRIM(CAST( bytd.schoolID AS VARCHAR(10)))) = sL.schoolID AND bytd.schoolType = sL.schoolType 
LEFT JOIN #BehaviorTypeQtr btype ON LTRIM(RTRIM(CAST( btype.schoolID AS VARCHAR(10)))) = sL.schoolID AND btype.schoolType = sL.schoolType 
LEFT JOIN #BehaviorRemovedStudentsCntQtr bsc ON LTRIM(RTRIM(CAST( bsc.schoolID AS VARCHAR(10)))) = sL.schoolID AND bsc.schoolType = sL.schoolType 
LEFT JOIN #MAPsList ml ON LTRIM(RTRIM(CAST( ml.schoolID AS VARCHAR(10)))) = sL.schoolID AND ml.schoolType = sL.schoolType 
--LEFT JOIN #MAPsGrowthList mg ON LTRIM(RTRIM(CAST( mg.schoolID AS VARCHAR(10)))) = sL.schoolID AND mg.schoolType = sL.schoolType 
--LEFT JOIN #MAPsProficientList mp ON LTRIM(RTRIM(CAST( mp.schoolID AS VARCHAR(10)))) = sL.schoolID AND mp.schoolType = sL.schoolType 
LEFT JOIN #GradeDistList g ON LTRIM(RTRIM(CAST( g.schoolID AS VARCHAR(10)))) = sL.schoolID AND g.schoolType = sL.schoolType 
LEFT JOIN #OnTrackList t ON LTRIM(RTRIM(CAST( t.schoolID AS VARCHAR(10)))) = sL.schoolID AND t.schoolType = sL.schoolType 
LEFT JOIN #APIB_List ai ON LTRIM(RTRIM(CAST( SL.schoolID AS VARCHAR(10)))) = ai.schoolID AND SL.schoolType = ai.schoolType
LEFT JOIN #CA_List ca ON LTRIM(RTRIM(CAST(SL.schoolID AS VARCHAR(10)))) = LTRIM(RTRIM(CAST(ca.CCSDLoc AS VARCHAR(10)))) AND SL. schoolType = ca.schoolType
WHERE edl.EducationOrganizationCurrentRowIndicator = 'Current'
GO


DROP TABLE dbo.QuarterlyProgressMonitoring_BehaviorIncidentsRanked;
SELECT 
	2022 AS schoolYear, 
	@CurrentTerm AS [Quarter], 
	* 
INTO dbo.QuarterlyProgressMonitoring_BehaviorIncidentsRanked
FROM #BehaviorQtrIncidentCountsRanked
GO
















/* Updating AP/IB/CTE/DC Partipation section... */

UPDATE sL
SET 
	sL.[Year1_AP_Perc] = ISNULL(ml.[Year1_AP_Perc], 0), 
	sL.[Year1_IB_Perc] = ISNULL(ml.[Year1_IB_Perc], 0), 
	sL.[Year1_DC_Perc] = ISNULL(ml.[Year1_DC_Perc], 0), 
	sL.[Year1_CTE_Perc] = ISNULL(ml.[Year1_CTE_Perc], 0), 
	sL.[Year1_Overall_Perc] = ISNULL(ml.[Year1_Overall_Perc], 0), 
	sL.[Year2_AP_Perc] = ISNULL(ml.[Year2_AP_Perc], 0), 
	sL.[Year2_IB_Perc] = ISNULL(ml.[Year2_IB_Perc], 0), 
	sL.[Year2_DC_Perc] = ISNULL(ml.[Year2_DC_Perc], 0), 
	sL.[Year2_CTE_Perc] = ISNULL(ml.[Year2_CTE_Perc], 0), 
	sL.[Year2_Overall_Perc] = ISNULL(ml.[Year2_Overall_Perc], 0), 
	sL.[Year3_AP_Perc] = ISNULL(ml.[Year3_AP_Perc], 0), 
	sL.[Year3_IB_Perc] = ISNULL(ml.[Year3_IB_Perc], 0), 
	sL.[Year3_DC_Perc] = ISNULL(ml.[Year3_DC_Perc], 0), 
	sL.[Year3_CTE_Perc] = ISNULL(ml.[Year3_CTE_Perc], 0), 
	sL.[Year3_Overall_Perc] = ISNULL(ml.[Year3_Overall_Perc], 0), 
	sL.[Year4_AP_Perc] = ISNULL(ml.[Year4_AP_Perc], 0), 
	sL.[Year4_IB_Perc] = ISNULL(ml.[Year4_IB_Perc], 0), 
	sL.[Year4_DC_Perc] = ISNULL(ml.[Year4_DC_Perc], 0), 
	sL.[Year4_CTE_Perc] = ISNULL(ml.[Year4_CTE_Perc], 0), 
	sL.[Year4_Overall_Perc] = ISNULL(ml.[Year4_Overall_Perc], 0)
--FROM #resultsCombined sL
FROM dbo.QuarterlyProgressMonitoring sL
INNER JOIN #APIB_List AS ml ON LTRIM(RTRIM(CAST( ml.schoolID AS VARCHAR(10)))) = sL.[CCSD#] AND ml.schoolType = sL.schoolType 





--SELECT * 
--INTO dbo.Backup_QuarterlyProgressMonitoring_20211115
--FROM dbo.QuarterlyProgressMonitoring

--SELECT * 
--INTO dbo.Backup_QuarterlyProgressMonitoring_20211102
--FROM dbo.QuarterlyProgressMonitoring



