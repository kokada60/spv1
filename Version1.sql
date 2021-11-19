SELECT * FROM dbo.MBK_20211116


DROP TABLE #temp0;
SELECT * INTO #temp0
FROM [dbo].[MBK_20211116] AS t
GO




DROP TABLE #enrollsNV
GO
SELECT * INTO #enrollsNV
FROM OPENQUERY(campus, '
	DECLARE @endYear  VARCHAR(10) = ''2022'';
	WITH calendars AS ( 
		SELECT DISTINCT
			c.calendarID, t.structureID, t.trialID, cc.value AS customCalendarValue, c.endYear, c.startDate calStartDate, c.endDate AS calEndDate,
			s.schoolID, s.standardCode ccsdID, s.number schoolNumber, ''02'' + LTRIM(RTRIM(CAST(s.number AS VARCHAR(10)))) AS stateSchoolID, s.name AS schoolName
		FROM clark.dbo.trial t 
		INNER JOIN clark.dbo.calendar c 
			ON COALESCE(t.active, ''0'') = ''1'' AND 			
			t.calendarID = c.calendarID
		INNER JOIN clark.dbo.scheduleStructure ss 
			ON ss.calendarID = c.calendarID AND ss.structureID = t.structureID 		
		INNER JOIN clark.dbo.school AS s
			ON c.schoolID = s.schoolID --AND ( NOT s.type = ''C''	) 
		INNER JOIN clark.dbo.customCalendar AS cc ON c.calendarID = cc.calendarID 
		INNER JOIN clark.dbo.campusAttribute AS ca ON ca.attributeID = cc.attributeID AND
			ca.object = ''calendar'' AND ca.element = ''ccsdtype''	
		WHERE 
			/*cc.value NOT IN (	
									/*For DF24 charters, Adult Eds, remote programs, and TMPs will not be excluded.*/	
									--''AD'',
									--''CH/MS'',''CH/HS'',''CH/ES'',''CH/JSHS'',	
									--''TMP'',''PGM''
									''SUM/J/SHS'',''SUM/CH/J/SHS'',''SUM/SEC/ESY'',''SUM/ES/ESY''
								) AND*/
			c.endYear = @endYear  
			
			--AND 
			--c.exclude = ''0'' 
			--AND 
			--c.summerSchool = ''0'' 		
	), enrollments AS (
		SELECT DISTINCT c.*, 
			e.enrollmentID, e.personID, p.studentNumber,
			i.lastName, i.firstName, i.birthDate, i.raceEthnicity,
			grade, e.serviceType, e.active, e.noShow, 
			eNV.foster,
			eNV.migratoryStatusDate,
			eNV.primaryNighttimeResidence, 
			CASE
					WHEN eNV.primaryNighttimeResidence IS NOT NULL
						THEN ''Y''
					ELSE ''N''
				END HomelessStatus,
			e.startDate, e.startStatus, 
			e.endDate, e.endStatus
		FROM calendars c
		INNER JOIN clark.dbo.enrollment e 
		INNER JOIN clark.dbo.person p ON e.personID = p.personID
			ON c.calendarID = e.calendarID AND c.structureID = e.structureID 			
			--AND e.startStatus NOT IN (''E4'', ''E5'', ''E6'', ''RC'')
			AND NOT e.serviceType = ''S''
			--AND ISNULL(e.noShow, ''0'') = ''0'' 
			--AND ISNULL(e.stateExclude, 0) = ''0''
			--AND e.serviceType = ''P''
			
		INNER JOIN clark.dbo.[identity] i ON p.currentIdentityID = i.identityID
		LEFT JOIN clark.dbo.enrollmentNV eNV ON e.enrollmentID = eNV.enrollmentID		
		WHERE e.endYear = @endYear  
	) 
	SELECT * 
	FROM enrollments
') 
GO


SELECT DISTINCT School FROM #temp0
EXCEPT 
SELECT DISTINCT schoolName FROM #enrollsNV
ORDER BY school 

DROP TABLE #CorrectedSchools;
SELECT * INTO #CorrectedSchools
FROM (
	SELECT 'Bozarth, Henry &amp; Evelyn ES' AS origSchoolName, 'Bozarth, Henry & Evelyn ES' AS CorrectedSchoolName UNION 
	SELECT 'Canarelli, Lawrence &amp; Heidi MS' AS origSchoolName, 'Canarelli, Lawrence & Heidi MS' AS CorrectedSchoolName UNION 
	SELECT 'Cram, Brian &amp; Teri MS' AS origSchoolName, 'Cram, Brian & Teri MS' AS CorrectedSchoolName UNION 
	SELECT 'Ellis, Robert &amp; Sandy ES' AS origSchoolName, 'Ellis, Robert & Sandy ES' AS CorrectedSchoolName UNION 
	SELECT 'Faiss, Wilbur &amp; Theresa MS' AS origSchoolName, 'Faiss, Wilbur & Theresa MS' AS CorrectedSchoolName UNION 
	SELECT 'Frias, Charles &amp; Phyllis ES' AS origSchoolName, 'Frias, Charles & Phyllis ES' AS CorrectedSchoolName UNION 
	SELECT 'Goolsby, Judy &amp; John L ES' AS origSchoolName, 'Goolsby, Judy & John L ES' AS CorrectedSchoolName UNION 
	SELECT 'Goynes, Theron H &amp; Naomi D ES' AS origSchoolName, 'Goynes, Theron H & Naomi D ES' AS CorrectedSchoolName UNION 
	SELECT 'Greenspun, Barbara &amp; Hank JHS' AS origSchoolName, 'Greenspun, Barbara & Hank JHS' AS CorrectedSchoolName UNION 
	SELECT 'Harney, Kathleen &amp; Tim MS' AS origSchoolName, 'Harney, Kathleen & Tim MS' AS CorrectedSchoolName UNION 
	SELECT 'Hayes, Keith C &amp; Karen W ES' AS origSchoolName, 'Hayes, Keith C & Karen W ES' AS CorrectedSchoolName UNION 
	SELECT 'Katz, Edythe &amp; Lloyd ES' AS origSchoolName, 'Katz, Edythe & Lloyd ES' AS CorrectedSchoolName UNION 
	SELECT 'Lowman, Mary &amp; Zel ES' AS origSchoolName, 'Lowman, Mary & Zel ES' AS CorrectedSchoolName UNION 
	SELECT 'Mannion, Jack &amp; Terry MS' AS origSchoolName, 'Mannion, Jack & Terry MS' AS CorrectedSchoolName UNION 
	SELECT 'Molasky, Irwin &amp; Susan JHS' AS origSchoolName, 'Molasky, Irwin & Susan JHS' AS CorrectedSchoolName UNION 
	SELECT 'Monaco, Mario C &amp; JoAnne MS' AS origSchoolName, 'Monaco, Mario C & JoAnne MS' AS CorrectedSchoolName UNION 
	SELECT 'Ober, D''Vorre &amp; Hal ES' AS origSchoolName, 'Ober, D''Vorre & Hal ES' AS CorrectedSchoolName UNION 
	SELECT 'Parson, Claude &amp; Stella ES' AS origSchoolName, 'Parson, Claude & Stella ES' AS CorrectedSchoolName UNION 
	SELECT 'Scherkenbach, William &amp; Mary ES' AS origSchoolName, 'Scherkenbach, William & Mary ES' AS CorrectedSchoolName UNION 
	SELECT 'Smalley, James E &amp; A Rae ES' AS origSchoolName, 'Smalley, James E & A Rae ES' AS CorrectedSchoolName UNION 
	SELECT 'Snyder, Don &amp; Dee ES' AS origSchoolName, 'Snyder, Don & Dee ES' AS CorrectedSchoolName UNION 
	SELECT 'Southwest Career &amp; Technical Academy HS' AS origSchoolName, 'Southwest Career & Technical Academy HS' AS CorrectedSchoolName UNION 
	SELECT 'Tarkanian, Lois &amp; Jerry MS' AS origSchoolName, 'Tarkanian, Lois & Jerry MS' AS CorrectedSchoolName UNION 
	SELECT 'West Career &amp; Technical Academy HS' AS origSchoolName, 'West Career & Technical Academy HS' AS CorrectedSchoolName 
	UNION 
	SELECT School AS origSchoolName, School AS CorrectedSchoolName
	FROM
	(
		SELECT DISTINCT School FROM #temp0
		INTERSECT
		SELECT DISTINCT schoolName FROM #enrollsNV
	) L0
) L1 
GO


DROP TABLE #temp1;
SELECT t.*, s1.CorrectedSchoolName, s1.ccsdID 
INTO #temp1 
FROM #temp0 t INNER JOIN ( 
	SELECT DISTINCT s.*, e.ccsdID, e.schoolNumber, e.stateSchoolID FROM #enrollsNV e INNER JOIN #CorrectedSchools s ON e.schoolName = s.CorrectedSchoolName
) s1 ON t.school = s1.origSchoolName 
GO


DROP TABLE #tmpFoster;
SELECT * INTO #tmpFoster FROM OPENQUERY(campus, '
	DECLARE @endYear VARCHAR(10) = ''2022''
	Select DISTINCT ps.studentNumber, pp.*
	from clark.dbo.programParticipation as pp
		inner join clark.dbo.program as p on p.programID = pp.programID
		inner join clark.dbo.Person as ps on ps.personID = pp.personID
		INNER JOIN clark.dbo.enrollment e ON ps.personID = e.personID 
	where e.endYear = @endYear
		and p.name = ''Foster Care''
')
GO

DROP TABLE #Foster;
GO
SELECT DISTINCT 
	o.[ID], 
	--o.[Incident Date],
	--o.[Infraction Date], 
	'Y' AS Foster 
INTO #Foster
FROM #temp1 o	
WHERE	EXISTS ( SELECT * FROM #tmpFoster tf0 WHERE tf0.studentNumber = o.ID 
				--AND 
				--o.[Incident Date] BETWEEN tf0.startDate AND ISNULL(tf0.endDate, o.[Incident Date]) 
				--o.[Infraction Date] BETWEEN tf0.startDate AND ISNULL(tf0.endDate, o.[Infraction Date]) 
			)
GO


DROP TABLE #Homeless;
SELECT DISTINCT e.ID, s.HomelessIndicator
INTO #Homeless 
FROM [ORION.CIS.CCSD.NET].SDM.dbo.DimStudent s 
INNER JOIN #temp1 e ON s.StudentNumber = e.ID 
WHERE s.StudentCurrentRowIndicator = 'Current' 
GO

SELECT * FROM #Homeless 
WHERE NOT HomelessIndicator = 'Not Homeless'



DROP TABLE #Migrant
GO
DECLARE @MaxDate DATE = CAST(GETDATE() AS DATE)--( SELECT MAX(EndDate) FROM #enrollsNV )
SELECT o.*, 'Y' AS [Migrant]
INTO #Migrant
FROM ( SELECT DISTINCT 
		ID, --AS [student #], --personID, 
			[Incident Date]
			--[Infraction Date] 
		FROM #temp1 ) o 
WHERE EXISTS ( SELECT * FROM #enrollsNV e 
				WHERE	
						--o.[student #] = e.studentNumber 
						o.ID = e.studentNumber 
						AND 
						CAST(o.[Incident Date] AS DATE) >= CAST(e.migratoryStatusDate AS DATE)
						--o.[Infraction Date] >= e.migratoryStatusDate 
					)
GO


DROP TABLE #tmpMilitary
GO
SELECT * 
INTO #tmpMilitary
FROM OPENQUERY(campus, 'SELECT p.studentNumber, m.* FROM clark.dbo.[v_studentmilitaryconnectionssummary] m INNER JOIN clark.dbo.person p ON m.personID = p.personID ') 
GO


DROP TABLE #tmpMilitary1
GO
DECLARE @MaxDate DATE = CAST(GETDATE() AS DATE)--( SELECT MAX(EndDate) FROM #enrollsNV )
SELECT * 
INTO #tmpMilitary1
FROM (
	SELECT *, ROW_NUMBER() OVER ( PARTITION BY o.ID,
									--@MaxDate AS [Incident Date],
									--o.[Infraction Date], 
									m.guardianPersonID 
								ORDER BY startDate DESC, ISNULL(endDate, @MaxDate) DESC ) cnt
	FROM ( SELECT DISTINCT 
				ID, 
				[Incident Date]
				--[Infraction Date] 
			FROM #temp1 ) o 
	INNER JOIN #tmpMilitary m 
		ON o.ID = m.studentNumber AND 
		o.[Incident Date] >= m.startDate
		--o.[Infraction Date] >= m.startDate
) L1 
WHERE cnt = 1 
GO


DROP TABLE #Military
GO
SELECT DISTINCT 
	ID, personID, 
	[Incident Date],
	--[Infraction Date], 
	'Y' AS Military 
INTO #Military
FROM #tmpMilitary1 m
WHERE NOT m.[status] = 'Discharged'
GO





DROP TABLE #FRLs
GO
DECLARE @DT DATE = CAST(GETDATE() AS DATE);
SELECT DISTINCT t.[Incident Date], t.ID , --f.eligibility AS FRL, 
	'Y' AS FRL
INTO #FRLs
FROM FRL.dbo.FRL f 
INNER JOIN #temp1 AS t ON t.ID = f.StudentNumber 
AND t.[Incident Date] BETWEEN f.StartDate AND f.EndDate
--AND t.[Infraction Date] BETWEEN f.StartDate AND f.EndDate
AND @DT BETWEEN f.RowEffectiveDate AND f.RowExpirationDate
WHERE f.endYear = '2022' AND f.Eligibility IN ( 'F', 'R' ) 
GO



DROP TABLE #FRLschoolsList;
SELECT DISTINCT f.*, s.[name] AS schoolName, 
	--REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(s.[Name], ', J T', ', J. T.'), 'Steele, Judith D ', 'Steele, Judith D. '),  '&', 'and'), ' ES', ' Elementary School'), ' MS', ' Middle School'), ' HS', ' High School') AS ModSchoolName 
	--REPLACE(s.[Name], 'Steele, Judith D ', 'Steele, Judith D. ') AS ModSchoolName 
	s.[Name] AS ModSchoolName
INTO #FRLschoolsList
FROM FRL.[dbo].[FRL Schools] AS f 
INNER JOIN campus.clark.dbo.school AS s ON f.[Local School Number] = s.standardCode 
WHERE f.[School Year] = '2021-2022'
GO




/* IEP */
DROP TABLE #tmpIEP
GO
SELECT * 
INTO #tmpIEP
FROM OPENQUERY(campus, '
	DECLARE @endYear VARCHAR(10) = ''2022'';	
	DECLARE @MaxDate DATE = CAST(GETDATE() AS DATE)--( SELECT MAX(EndDate) FROM #enrollsNV );

	SELECT
		p.personID
		, p.studentNumber
		,[iep] = ''Y''
		,specialEdStatus
		,specialEdSetting
		,disability1
		,startDate
		,endDate
		,locked
		,a.typeID			
	FROM [clark].[dbo].[plan] a
	INNER JOIN clark.dbo.planType pt ON a.typeID = pt.typeID
	INNER JOIN clark.dbo.person p ON a.personID = p.personID
	--INNER JOIN clark.dbo.enrollment e ON p.personID = e.personID 
	JOIN [clark].[dbo].[planState] b
		ON  b.planID = a.planID 
	WHERE 
		--e.endYear = @endYear AND
		ISNULL(a.locked, 0) = 1 AND 
		pt.module = ''Specialed'' AND
		a.typeid != 21 --AND	--Excludes 504s			
		--(
		--	@MaxDate BETWEEN a.startDate AND a.endDate
		--	--( a.startDate BETWEEN @StartDate AND @EndDate AND a.startDate <= a.endDate ) OR 
		--	--( a.endDate BETWEEN @StartDate AND @EndDate AND a.startDate <= a.endDate ) OR 
		--	--( @StartDate BETWEEN a.startDate AND a.endDate AND @StartDate <= @EndDate ) OR 
		--	--( @endDate BETWEEN a.startDate AND a.endDate AND @StartDate <= @EndDate ) OR 
		--	--( @StartDate BETWEEN a.startDate AND a.endDate AND @EndDate BETWEEN a.startDate AND a.endDate AND a.startDate <= a.endDate ) OR 
		--	--( a.startDate BETWEEN @StartDate AND @EndDate AND a.endDate BETWEEN @StartDate AND @EndDate AND a.startDate <= a.endDate ) 
		--) 				
') 
GO



DROP TABLE #tmpIEPFiltered;
SELECT DISTINCT i.* 
INTO #tmpIEPFiltered
FROM #temp1 s 
INNER JOIN #tmpIEP i ON s.ID = i.studentNumber 
GO

SELECT * FROM #tmpIEP
WHERE studentNumber = '1048104'

SELECT * FROM #tmpIEPFiltered 
WHERE studentNumber = '1048104'
ORDER BY endDate DESc, startDate DESC 

SELECT * FROM #temp1 WHERE ID = '1048104'

DECLARE @MaxDate DATE = CAST(GETDATE() AS DATE)--( SELECT MAX(EndDate) FROM #enrollsNV )
DROP TABLE #IEPs;
SELECT DISTINCT 
	--t.personID, 
	t.ID,
	t.[Incident Date], iL.*
	--t.[Infraction Date], iL.*
INTO #IEPs
FROM #temp1 t 
CROSS APPLY ( 
	SELECT TOP 1
		IEP, startDate, endDate
	FROM #tmpIEPFiltered i
	WHERE	i.studentNumber = t.ID AND (
				t.[Incident Date] BETWEEN i.startDate AND i.endDate OR -- incident date overwrap.
				CAST(t.[Start Date] AS DATETIME) BETWEEN i.startDate AND i.endDate OR
				CAST(t.[End Date] AS DATETIME) BETWEEN i.startDate AND i.endDate OR 
				i.startDate BETWEEN CAST(t.[Start Date] AS DATETIME) AND CAST(t.[End Date] AS DATETIME) OR 
				i.endDate BETWEEN CAST(t.[Start Date] AS DATETIME) AND CAST(t.[End Date] AS DATETIME) 
			) 
			--AND t.[Infraction Date] BETWEEN i.startDate AND i.endDate 
	ORDER BY i.endDate DESC, i.startDate DESC 
) iL
GO



DROP TABLE #tmpLEP
GO
SELECT * 
INTO #tmpLEP
FROM OPENQUERY(campus, '	
	SELECT p.studentNumber, i.personID, programStatus, identifiedDate, expectedExitDate, exitDate, modifiedDate FROM [clark].[dbo].[lep] i 
	INNER JOIN clark.dbo.person p ON i.personID = p.personID
')
GO


DROP TABLE #tmpLEPs_2;
GO
SELECT DISTINCT 
	*
INTO #tmpLEPs_2
FROM (	
	SELECT DISTINCT 
		--s.personID, 
		s.ID,
		s.[Incident Date], 
		--s.[Infraction Date], 
		t.identifiedDate, 
		t.[exitDate], t.modifiedDate, t.programStatus
	FROM #temp1 s
	INNER JOIN #tmpLEP t 
		--ON s.personID = t.personID 
		ON s.ID = t.studentNumber
	WHERE 
		(
			t.exitDate IS NULL OR 
			( 
				t.exitDate IS NOT NULL AND 
				t.exitDate  >= s.[Incident Date] 
				--t.exitDate  >= s.[Infraction Date] 
			) 
		) AND 
		( 
			t.identifiedDate IS NULL 
			OR (
				t.identifiedDate IS NOT NULL 
				AND t.identifiedDate <= s.[Incident Date]
				--AND t.identifiedDate <= s.[Infraction Date]
			) 
		) 
) L1 
GO


DROP TABLE #LEPs;
SELECT DISTINCT 
	--personID, 
	ID,
	[Incident Date], 
	--[Infraction Date], 
	programStatus, exitDate 
INTO #LEPs
FROM (
	SELECT lL.* 
	FROM ( SELECT DISTINCT 
				--personID, 
				ID,
				[Incident Date], 
				--[Infraction Date], 
				[identifiedDate] 
			FROM #tmpLEPs_2 ) l0 
	CROSS APPLY ( 
		SELECT TOP 1 * FROM #tmpLEPs_2 l1
		WHERE 
			--l0.personID = l1.personID  AND 
			l0.ID = l1.ID AND
			l0.[Incident Date] = l1.[Incident Date]
			--l0.[Infraction Date] = l1.[Infraction Date]
		ORDER BY CASE WHEN exitDate IS NULL THEN 1 ELSE 0 END DESC, exitDate DESC, CASE WHEN modifiedDate IS NULL THEN 1 ELSE 0 END DESC, modifiedDate DESC, CASE WHEN identifiedDate IS NUlL THEN 1 ELSE 0 END DESC, identifiedDate DESC
	) lL 
) L1 
WHERE ISNULL(programStatus, '') = 'LEP' 
GO


SELECT DISTINCT ID FROM #temp1 
EXCEPT
SELECT DISTINCT ID FROM #LEPs


DROP TABLE #Flags;
SELECT 
	StudentNumber,  EducationOrganizationID, EducationOrganizationName, ServiceTypeCode, 
	EntryDateCalendarName, EntryDateFullDate, ExitDateFullDate, 
	schoolYear, GradeLevelCode, IEPIndicator, IEPEntryDate, IEPExitDate, LEPCode, LEPIndicator, LEPEntryDate, LEPExitDate, 
	Section504Indicator, Section504EntryDate, Section504ExitDate, FRLIndicator
INTO #Flags 
FROM SDM.curr.FactStudentEnrollmentCombined AS f0
WHERE f0.schoolYear IN (2022)
GO

DROP TABLE #result;
SELECT 
	t.[ID], 
	t.[Last Name], 
	t.[First Name], 
	t.[Grade], 
	t.[Gender], 
	t.[Ethnicity], 
	t.[Incident Date], 
	t.[Offense], 
	t.[Resolution], 
	t.[Duration], 
	t.[Start Date], 
	t.[Start Timestamp], 
	t.[End Date], 
	t.[End Timestamp], 
	t.[School], 
	t.[Region], 
	t.[CCSD Type], 
	t.[Role], 
	t.[Bully Type], 
	--t.[CorrectedSchoolName], 
	--t.[ccsdID], 
	CASE WHEN EXISTS ( SELECT * FROM #IEPs iep WHERE iep.ID = t.ID AND iep.[Incident Date] = t.[Incident Date] AND ISNULL(iep.[IEP], 'N') = 'Y' ) 
		THEN '1' ELSE '0' END AS [IEP], 
	CASE WHEN EXISTS ( SELECT * FROM #Flags f WHERE f.schoolYear = '2022' AND t.ID = f.studentNumber AND f.Section504Indicator = '504' ) 
		THEN '1' ELSE '0' END AS [504],
	CASE WHEN EXISTS ( SELECT * FROM #LEPs lp WHERE t.ID = lp.ID AND t.[Incident Date] = lp.[Incident Date] AND lp.programStatus = 'LEP' ) 
		THEN '1' ELSE '0' END AS [LEP], 
	CASE WHEN	EXISTS ( SELECT * FROM #FRLs fL WHERE fL.ID = t.ID AND t.[Incident Date] = fL.[Incident Date] AND FRL = 'Y' ) OR
				--EXISTS ( SELECT * FROM #FRLschoolsList sL WHERE sL.[Local School Number] = t.CCSD ) 
				EXISTS ( SELECT * FROM #FRLschoolsList sL WHERE sL.[Local School Number] = t.ccsdID ) 				
			THEN '1' ELSE '0' END AS [FRL], 
	CASE WHEN EXISTS ( SELECT * FROM #Foster fL WHERE fL.ID = t.ID AND Foster = 'Y' ) THEN '1' ELSE '0' END AS [Foster], 
	CASE WHEN EXISTS ( SELECT * FROM #Homeless hL WHERE hL.ID = t.ID AND NOT HomelessIndicator = 'Not Homeless' ) THEN '1' ELSE '0' END AS [Homeless], 
	CASE WHEN EXISTS ( SELECT * FROM #Migrant AS mg WHERE mg.ID = t.ID AND Migrant = 'Y' ) THEN '1' ELSE '0' END AS [Migrant], 
	CASE WHEN EXISTS ( SELECT * FROM #Military mL WHERE mL.ID = t.ID AND mL.Military = 'Y' ) THEN '1' ELSE '0' END AS [Military]
INTO #result
FROM #temp1 AS t
GO

SELECT * FROM #result 
SELECT SUM([IEP]) --, SUM([504]) --, SUM(LEP), SUM(FRL), SUM(Foster), SUM(Homeless), SUM(Migrant), SUM(Military) 
FROM #result

SELECT DISTINCT [IEP] FROM #result 
SELECT DISTINCT [504] FROM #result 
SELECT DISTINCT [LEP] FROM #result 
SELECT DISTINCT [FRL] FROM #result 
SELECT DISTINCT [Foster] FROM #result 
SELECT DISTINCT [Migrant] FROM #result 
SELECT DISTINCT [Military] FROM #result 
SELECT DISTINCT [Homeless] FROM #result 
SELECT COUNT(*), COUNT(DISTINCT ID)  FROM #temp0
SELECT COUNT(*), COUNT(DISTINCT ID)  FROM #result
