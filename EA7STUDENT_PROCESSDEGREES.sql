USE [BRT-10007578]
GO

/****** Object:  StoredProcedure [dbo].[EA7STUDENT_PROCESSDEGREES]    Script Date: 7/30/2025 2:55:11 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

 /*
Description:   This procedure processes the requirements for all degrees
Input:         @EA7STUDENTSID - ID of student to be processed, value should be primary key from EA7STUDENTS table
Output:        Nothing is returned

Revision History:
01/03/2007 SAM CR262099-122006 - SELECT TOP 1 ... ORDER BY... produced unexpected results with the order not always being correct.
    Made changes to set order before looping through table variable.
02/07/2007 SAM CR265009-012907 - Added statement to remove global alternates that are not enrolled, but all other requests
    for that session are.
04/09/2007 SAM CR271534-033007 - Changed the statement that deleted multiple courses that did not award credit.  It was ordering
    by awarded credit, then attempted credit, but this was unnecessary and in this case produced undesirable results.  The goal was
    to have the one instance that did award credit be at the top so we could do a "select top 1", and then delete the rest since they
    shouldn't award credit.  It was not anticipated that a course marked to not award credit multiple times would be forced to award credit
    multiple times by adding it as an addtional course.  It now orders by awarded credit and grade value (or num. equivelant) to make 
    sure the instance that awards credit and gives the highest grade is used, and the extra instances are not.
04/12/2007 SAM CR272680-041207 - Corrected a join statement error found by Kirby
04/17/2007 JAY CR271422-032907 - Changed an SQL statement for performance (search for CR number for more info.)
05/04/2007 SAM CR274460-050307 - Made changes to better handle courses taken multiple times for credit.  Search for CR number for more info.
05/07/2007 SAM CR274460-050307 & CR274547-050407 - Made changes to store individual statuses for courses to better handle multiple courses.
    New status field is in table EA7STUDENTDEGREQCOURSELINKS.  
05/10/2007 SAM CR271422-032907 - Removed a couple SQL statements that were no longer used
05/14/2007 SAM CR274460-050307 - changes for courses taken for credit multiple times.  search for CR number for more info
05/21/2007 SAM CR275707-052107 - made changes to handle 0 versus null in grades table
09/05/2007 SAM CR282285-090407 - inner join was elimiting records we needed for calculating credits possible
10/15/2007 SAM - Getting 'Ambigous column' error for DEGREETYPE when updating statuses at the end.  Another field of the same name
    has been added to EA7DEGREES table, so the statements below needed the alias for EA7STUDENTDEGREES added below.
03/25/2008 SAM CR295384-030708 - Getting a constraint error because of an unexpected negative value.  see below for details
02/19/2008 QW  CR289983-121207 - If the translation table has no numeric equivalent defined, gradevalue could be null, we need to sort by creditsawarded
03/24/2008 EL CR295631-031108 - Need to order by sequence in the degree info table so courses fulfill the degree in the correct order if the order is changed after it's been saved already
04.25.2008 mhr 7.77 Repeat Course Changes - replace EA7Courses.AllowMultipleCredit with EA7StudentCourses.RepeatAllowed
08.13.08 CR304778-071608 we should only be modifying requirements of this student
08.26.08 CR306801-082008 This change is to fix the scenario where a course should not be listed because it satisfies a requirement above and its 'can satisfy' field is set to 'only this requirement'
08.26.08 AlisonBu CR307989-091008,CR308756-092308 - Only 'No waitlist' or 'enrolled from waitlist' courses should be included
09.10.08 CR307890-090908 Make sure substituted credits get counted
09.16.08 CR308138-091108 Other course was awarded credit, but a regular course was also awarded credit, so added @CurrentCredits to creditstouse even if there currentothercredits > 0
09.17.08 jkl (CR308444-091708) when setting degree complete, check for completed major/minors was not aliased correctly
09.22.08 CR308394-091708 if we are not using this course to satisfy this requirement, remove it from the temp cache table that keeps track of different can-satisfy courses. 
09.30.08 CR308689-092208 Set @ExtraCourseID to 0 if returns null so it doesn't stay in an infinite loop
10.03.08 CR307580-090408 There are two problems: 1) negative values were being set for the creditused field here. 2) Set creditapplied change is preventative, this field is not set correctly
10.07.08 CR307580-090408 Additional problems setting negative values
10.29.08 CR308138-091108 Per design, all studentcourse instances that award credit should be counted. Created temp table to reorder @studentcourseinfo table for CR271534-033007
10.29.08 CR310473-102408 left join lists too many multiple courses
11.28.08 CR310381-102308 other course that has not been awarded credit will be listed as planned
01.29.09 CR313684-011209 do not include 'Enrolled from waitlist' student courses that have been dropped
02.11.09 CR314983-021009 set completedon field only for degrees and college
01.29.09 CR315059-021109 each course with multiple intances needs to be considered; before it was only returning the top 1 of all courses with multiple instances
02.19.09 CR315304-021809 issues with other courses - counting otherCreditsUsed and CurrentOtherCredits
02.20.09 CR315304-021809 issues with other courses - multiple courses table counting other courses credits as more than used
02.24.09 CR315526-022309 Instances not awarding credits should be considered as additional courses; unless none of the instances award credit
04.28.09 CR318270-041709 Courses with credits (Other schools courses including) take precedence over Planned or Inprogress. Confirmed by Design.
05.12.09 CR317548-040609 If a course gets bumped out after credits were considered and then removed because it did not meet group requirment, we need to remove it from @TempCachedCourseLinks also after removing from EA7STUDENTDEGREEREQCOURSES
06.11.09 ScottKa 7.82 Development - Zero Credit Courses
06.17.09 ScottKa 7.82 Development - Zero Creidt Courses - Changes for courses with grades and don't recieve credit 
06.22.09 7.82 DEV What-If Scenario
06.26.09 AlisonBu 7.82 Dev - Minimum grade requirements
08.11.09 ScottKa CR323466-081009 - For "Other" credits if the number of credits awarded is greater than the number of credits used, then OtherCreditsUsed will contain the number of credits used
MERGED FROM 7.80_Patch: 06.15.09 CR319525-051409 other credits used should be counted by degree, confirmed by design
09.23.09 CR324817-092109 Per design, moving withdrawn courses to additional courses
10.15.09 CR325537-101409 MaryRu  Added "OR IsOtherCourse" to counteract change for CR324817-092109 which always excludes Withdrawn and OtherCourses when we really just want to exclude Withdrawn
10.15.09 CR325494-101409 Need to show withdrawn courses if credits awarded
10.15.09 CR325520-101409 MaryRu  Check that grade exists for Task Requirements was incorrect.
06.09.10 CR328888-032510 ThomasRa Added check to Min Grade restrictions for classes without grades.
07.08.10 CR330211-061710 ShirleyHs When comparing minimum grade requirements, and the student grade is a numeric grade, use the cutoff value of the translation entry to compare
03.17.11 CR334176-020711 Priyanka Added Order by clause 
05.02.11 CR335708-041311 MaryRu  respect order of courses when processing
10.13.11 CR338421-082211 ShirleyHs if there's a term that's not graded after graded terms for a course, then count that course as in progress; added partial credit field to track and sort
01.10.12 CR341362-020812 ShirleyHs Need to reset minimum grade variables that could be set to null
02.15.12 CR341302-020312 ShirleyHs Course with grade awarding credit but grade is 0 and does not meet min grade requirement should not pass min grade check
02.16.12 CR341419-021012 ShirleyHs Variable needs to be reset for can satisfy check to be accurate
02.28.12 CR341574-022112 ShirleyHs only sort by partial credit when no credits are awarded for all instances of a course
09.05.12 CR337746-071511 QiongWu Add Previous Status column, when its value is Completed, we will not add history record in TR_EA7STUDENTDEGREES_UPD.
09.12.12 CR344938-083112 ShirleyHs restrict multiple instances of the same course that don't meet the min grade later in the processing (CR340150-112211)
10.10.12 CR343639-061212 QiongWu, if there's a term for this course that needs a grade and the student also enrolled the class in this term, don't award credit for the course yet, count it as in progress (until all terms have grades). If the student does not enroll the class in this term or withdraws from the class in this term, we will award credit for the course in the previous term(s).
02.22.13 CR343238-051812 ShirleyHs, Check groups minimum required credits and credits needed to correctly; Also, only use filters when using specific groups
08.19.15 User Story 493112:NC7/Error: Transaction (process ID 117) was deadlocked on lock resources when enrolling in classes
*/


CREATE PROCEDURE [dbo].[EA7STUDENT_PROCESSDEGREES]
(
    @EA7STUDENTSID INT,
    @bIsWhatIf smallint = 0
)
AS
BEGIN
BEGIN TRY

	DECLARE @returnCode INT

	BEGIN TRANSACTION
	SET NOCOUNT ON
	--Naveen 07/07/08 CR303219-062308
        SET ANSI_WARNINGS OFF

	
	EXEC @returnCode = sp_getapplock  
		 @Resource = 'EA7STUDENT_PROCESSDEGREES',  
		 @LockMode = 'Update',  
		 @LockOwner = 'Transaction',  
		 @LockTimeout = 0, 
		 @DbPrincipal  = 'public' 

    IF @returnCode NOT IN (0, 1)
       BEGIN 
        RAISERROR ( 'Unable to acquire Update Lock on EA7STUDENT_PROCESSDEGREES', 16, 1 )         
	   RETURN 
		END 

	DECLARE @TE_MARKINGCOLUMN INT,@TOTALEXEMPTED numeric(19,7)
	declare @tint_IsCollege tinyint --CR314983-021009
	
	--Get Marking Column Table Entry to use
	SELECT @TE_MARKINGCOLUMN=COALESCE(NUMBER, 0)
	FROM BUSINESSRULESDATA
	WHERE BUSINESSRULENUMBER = 749

	--clear everything and start over, cascades to EA7STUDENTDEGREQCOURSELINKS
	DELETE EA7STUDENTDEGREEREQCOURSES
	FROM EA7STUDENTDEGREEREQCOURSES SDRC
	INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDRC.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
	INNER JOIN EA7STUDENTDEGREES SD ON SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID
	WHERE SD.EA7STUDENTSID = @EA7STUDENTSID AND SD.COMPLETEDON IS NULL AND SD.ISWHATIF = @bIsWhatIf

	--clear group table
	DELETE EA7STUDENTDEGREEREQGROUPS
	FROM EA7STUDENTDEGREEREQGROUPS SDRG
	INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDRG.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
	INNER JOIN EA7STUDENTDEGREES SD ON SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID
	WHERE SD.EA7STUDENTSID = @EA7STUDENTSID AND SD.COMPLETEDON IS NULL AND SD.ISWHATIF = @bIsWhatIf

	UPDATE EA7STUDENTDEGREEREQS
	SET STATUS= 1
	FROM EA7STUDENTDEGREEREQS SDR
	INNER JOIN EA7STUDENTDEGREES SD ON SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID
	WHERE SD.EA7STUDENTSID = @EA7STUDENTSID AND SD.COMPLETEDON IS NULL AND SD.ISWHATIF = @bIsWhatIf
	
	UPDATE EA7STUDENTDEGREES
	SET PREVIOUSSTATUS = STATUS
	WHERE EA7STUDENTSID = @EA7STUDENTSID AND COMPLETEDON IS NULL AND EA7STUDENTDEGREES.ISWHATIF = @bIsWhatIf

	UPDATE EA7STUDENTDEGREES
	SET STATUS = 1
	WHERE EA7STUDENTSID = @EA7STUDENTSID AND COMPLETEDON IS NULL AND EA7STUDENTDEGREES.ISWHATIF = @bIsWhatIf

	DECLARE @CalculateExemptionsSubstitutions TABLE 
	(
		CalculateExemptionsSubstitutionsID int identity(1,1),
		EA7STUDENTDEGREEREQSID int,
		CREDITSREQUIRED numeric(19,4),
		CREDITSREQUIREDTO numeric(19,4),
		DIFF numeric(19,4),
		PreviousTotal numeric(19,4),
		EA7STUDENTDEGREESID int
	)

	DECLARE @STUDENTDEGREEINFO TABLE
	(
		STUDENTDEGREEINFOID int identity(1,1),
		EA7STUDENTDEGREESID int,
		EA7STUDENTDEGREEREQSID int,
		EA7DEGREEREQCOURSESID int,
		EA7COURSESID int,
		CREDITSREQUIRED numeric(19,4),
		CREDITSREQUIREDTO numeric(19,4),
		CANSATISFY smallint,
		DEGREE_PRIORITY int,
		REQUIREMENT_PRIORITY int,
		CREDITSPOSSIBLE numeric(19,4),
		CREDITSAWARDED numeric (19,4), --CR318270-041709
		EA7DEGREESID int,
		CREDITSSUMBYREQUIREMENT numeric(19,4),
		HIGHESTGRADE numeric(19,7),
		GENERALELECTIVE smallint,
		GROUPID int,
		NUMBERGROUPSREQUIRED int,
		REQUIREMINIMUMGRADE smallint,
		MINIMUMGRADEVALUE numeric(19,7),
		MINIMUMGRADE_TE varchar(10),
		DRCSEQUENCE int,				--CR335708-041311
		PARTIALCREDIT numeric(19, 4)	--CR338421-082211		
	)
	
	DECLARE @ExemptedSubstitutedTotals TABLE 
	(
		EA7STUDENTDEGREESID int,
		TotalExemptedSubstitutedCredits numeric(19,4),
		TotalUnappliedCredits numeric(19,4),
		PROCESSED int
	)

	DECLARE @CanSatisfyLinks TABLE
	(
		STUDENTDEGREEINFOID int,
		EA7STUDENTDEGREEREQCOURSESID int
	)

	DECLARE @TempCachedCourseLinks TABLE
	(
		EA7STUDENTDEGREEREQCOURSESID int, 
		EA7STUDENTCOURSESID int, 
		STATUS int, 
		CREDITSUSED numeric(19,4),
		CANSATISFY smallint
	)

	DECLARE @STUDENTCOURSEINFO TABLE 
	(
		EA7STUDENTCOURSESID int,
		EA7COURSESID int,
		CREDITSATTEMPTED numeric(19,4),
		CREDITSAWARDED numeric (19,4),
		OTHERCOURSE smallint,
		EA7SESSIONSID int,
		GRADEVALUE numeric(19,7),			--Coalesced GradeValue from EA7StudentGrades and NumericEquivalent of translated EA7TranslationEntriesID from EA7StudentGrades
		REPEATALLOWED smallint,
		ISALTERNATE smallint,
		STUDENTCOURSELINK int,
		EA7TRANSLATIONSID int,				--EA7TranslationsID from EA7CourseGradingInfoGrades - this is a translation table, not a grade in the table
		TTSEQUENCE int,
		EA7SG_GRADEVALUE numeric(19,7),		--actual GradeValue field from EA7StudentGrades         --added for CR325520-101409
		EA7SG_EA7TRANSLATIONENTRIESID int,	--actual EA7TranslationEntriesID from EA7StudentGrades  --added for CR325520-101409
		GRADETYPE int,						--CR330211-061710
		CREDITEARNED smallint,				--CR334176-020711 
		PARTIALCREDIT numeric(19, 4)		--CR338421-082211
	)
	
	--CR308138-091108	reorder temp table for CR271534-033007
	DECLARE @TEMPSTUDENTCOURSEINFO TABLE 
	(
		EA7STUDENTCOURSESID int,
		EA7COURSESID int,
		CREDITSATTEMPTED numeric(19,4),
		CREDITSAWARDED numeric (19,4),
		OTHERCOURSE smallint,
		EA7SESSIONSID int,
		GRADEVALUE numeric(19,7),			--Coalesced GradeValue from EA7StudentGrades and NumericEquivalent of translated EA7TranslationEntriesID from EA7StudentGrades
		REPEATALLOWED smallint,
		ISALTERNATE smallint,
		STUDENTCOURSELINK int,
		EA7TRANSLATIONSID int,				--EA7TranslationsID from EA7CourseGradingInfoGrades - this is a translation table, not a grade in the table
		TTSEQUENCE int,
		EA7SG_GRADEVALUE numeric(19,7),		--actual GradeValue field from EA7StudentGrades
		EA7SG_EA7TRANSLATIONENTRIESID int,	--actual EA7TranslationEntriesID from EA7StudentGrades
		GRADETYPE int,						--CR330211-061710
		CREDITEARNED smallint,				--CR334176-020711 
		PARTIALCREDIT numeric(19, 4)		--CR338421-082211
	)	
	
	--fill degree info table variable
	--EL CR295631-031108 03/24/08 Need to order by sequence so courses fulfill the degree in the correct order
	--MaryRu CR335708-041311 From above CR we were adding to this table in the correct order, but we weren't using this order when 
	--    processing way, way below. Added DRCSequence to the table so we can utilize this order later.
	INSERT INTO @STUDENTDEGREEINFO(EA7STUDENTDEGREESID, EA7STUDENTDEGREEREQSID, EA7DEGREEREQCOURSESID, 
		EA7COURSESID, CREDITSREQUIRED, CREDITSREQUIREDTO, CANSATISFY, DEGREE_PRIORITY, REQUIREMENT_PRIORITY,
		EA7DEGREESID, GENERALELECTIVE, GROUPID, NUMBERGROUPSREQUIRED, REQUIREMINIMUMGRADE, MINIMUMGRADEVALUE, MINIMUMGRADE_TE, DRCSEQUENCE)
	SELECT SD.EA7STUDENTDEGREESID, SDR.EA7STUDENTDEGREEREQSID, DRC.EA7DEGREEREQCOURSESID, 
		DRC.EA7COURSESID, DR.CREDITSNEEDED, DR.CREDITSNEEDEDTO, DRC.CANSATISFY,
		D.PROCESSORDER, DR.PRIORITY, D.EA7DEGREESID, DR.GENERALELECTIVE, DRC.GROUPID,
		CASE DR.REQUIREMENTFULFILLEDTYPE WHEN 1 THEN -1 ELSE DR.NUMBEROFGROUPS END,
		DR.REQUIREMINIMUMGRADE, DR.MINGRADEVALUE, DR.MINGRADETRANSLATIONENTRY, DRC.SEQUENCE
	FROM EA7STUDENTDEGREES SD 
	INNER JOIN EA7STUDENTDEGREEREQS SDR ON SD.EA7STUDENTDEGREESID = SDR.EA7STUDENTDEGREESID
	INNER JOIN EA7DEGREES D ON SD.EA7DEGREESID = D.EA7DEGREESID
	INNER JOIN EA7DEGREEREQUIREMENTS DR ON SDR.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID
	INNER JOIN EA7DEGREEREQCOURSES DRC ON DR.EA7DEGREEREQUIREMENTSID = DRC.EA7DEGREEREQUIREMENTSID
	WHERE SD.EA7STUDENTSID = @EA7STUDENTSID AND SD.COMPLETEDON IS NULL AND SD.ISWHATIF = @bIsWhatIf
	ORDER BY SD.EA7STUDENTSID, DRC.EA7DEGREEREQUIREMENTSID, DRC.SEQUENCE

	--fill table variable with info on what courses the student has taken so far
	INSERT INTO @TEMPSTUDENTCOURSEINFO(EA7STUDENTCOURSESID, EA7COURSESID, CREDITSATTEMPTED, CREDITSAWARDED, OTHERCOURSE, EA7SESSIONSID, REPEATALLOWED, ISALTERNATE, STUDENTCOURSELINK, GRADEVALUE, EA7TRANSLATIONSID, TTSEQUENCE, EA7SG_GRADEVALUE, EA7SG_EA7TRANSLATIONENTRIESID, GRADETYPE, CREDITEARNED)
	select sc.ea7studentcoursesid, sc.ea7coursesid, dbo.sf_EA7GetAttemptedCreditsByEA7StudentCoursesID(sc.EA7STUDENTCOURSESID), 
		dbo.sf_EA7GetAwardedCreditsByEA7StudentCoursesID(sc.EA7STUDENTCOURSESID), 
		SC.ISOTHERCOURSE,
		SC.EA7SESSIONSID, SC.RepeatAllowed, SC.ISALTERNATE, sc.STUDENTCOURSELINK,
		COALESCE(SGTE.NUMEQUIVALENT, SGTE.GRADEVALUE),
		(Select EA7TRANSLATIONSID From EA7COURSEGRADINGINFOGRADES CGIG INNER JOIN EA7MARKINGCOLUMNS MC ON CGIG.EA7MARKINGCOLUMNSID = MC.EA7MARKINGCOLUMNSID where CGIG.EA7COURSEGRADINGINFOID = CGI.EA7COURSEGRADINGINFOID and MC.DESCRIPTION = @TE_MARKINGCOLUMN ),
		SGTE.SEQUENCE,
		SGTE.GRADEVALUE,
		SGTE.EA7TRANSLATIONENTRIESID,
		SGTE.GRADETYPE,
		SGTE.CREDITEARNED
	from ea7studentcourses sc
	INNER JOIN EA7COURSES C ON sc.EA7COURSESID = C.EA7COURSESID
	LEFT OUTER JOIN EA7COURSEGRADINGINFO CGI ON C.EA7COURSESID = CGI.EA7COURSESID AND SC.EA7SESSIONSID = CGI.EA7SESSIONSID
	LEFT OUTER JOIN TABLEENTRIES ET ON sc.enrollmenttype = ET.TABLEENTRIESID
	LEFT OUTER JOIN (SELECT TE.NUMEQUIVALENT, SG.GRADEVALUE, TE.SEQUENCE, SG.EA7STUDENTCOURSESID, SG.EA7TRANSLATIONENTRIESID, SG.GRADETYPE, TE.CREDITEARNED FROM EA7STUDENTGRADES SG LEFT OUTER JOIN EA7TRANSLATIONENTRIES TE ON SG.EA7TRANSLATIONENTRIESID = TE.EA7TRANSLATIONENTRIESID INNER JOIN EA7MARKINGCOLUMNS MC ON SG.EA7MARKINGCOLUMNSID = MC.EA7MARKINGCOLUMNSID WHERE MC.DESCRIPTION = @TE_MARKINGCOLUMN) SGTE on SC.EA7STUDENTCOURSESID = SGTE.EA7STUDENTCOURSESID
	where sc.ea7studentsid = @EA7STUDENTSID and coalesce(ET.STATICENTRYID,0) <> 2	--exlude audited courses
	and ((sc.ISOTHERCOURSE = 0 AND CGI.EA7COURSEGRADINGINFOID IS NOT NULL) OR (SC.ISOTHERCOURSE = -1))
	and (sc.WaitlistStatus = 1 OR (sc.WAITLISTSTATUS = 5 AND (sc.REQUEST = -1 OR coalesce(sc.ENROLLEDTERMS,0) > 0))) --CR313684-011209 JamieEl & CR307989-091008,CR308756-092308 Only 'No waitlist' or 'enrolled from waitlist' should be included
	--CR324817-092109 Per design, moving withdrawn courses to additional courses
    --and ((sc.COURSELENGTHINTERMS > sc.WITHDRAWNTERMS) and (sc.COURSELENGTHINTERMS <> 0))
	--MaryRu 10.15.2009 CR325537-101409  Added "OR IsOtherCourse" since the TermNumber checks will always exclude them.
	and ((((sc.COURSELENGTHINTERMS > sc.WITHDRAWNTERMS) and (sc.COURSELENGTHINTERMS <> 0)) OR (SC.ISOTHERCOURSE = -1))
  	  --PS 10.15.2009 CR325494-101409
	  OR ((sc.COURSELENGTHINTERMS = sc.WITHDRAWNTERMS) AND (sc.COURSELENGTHINTERMS <> 0) AND (dbo.sf_EA7GetAwardedCreditsByEA7StudentCoursesID(sc.EA7STUDENTCOURSESID) > 0)))
    
	--CR308138-091108	reorder temp table for CR271534-033007
	INSERT INTO @STUDENTCOURSEINFO(EA7STUDENTCOURSESID, EA7COURSESID, CREDITSATTEMPTED, CREDITSAWARDED, OTHERCOURSE, EA7SESSIONSID, REPEATALLOWED, ISALTERNATE, STUDENTCOURSELINK, GRADEVALUE, EA7TRANSLATIONSID, TTSEQUENCE, EA7SG_GRADEVALUE, EA7SG_EA7TRANSLATIONENTRIESID, GRADETYPE, CREDITEARNED)
	SELECT EA7STUDENTCOURSESID, EA7COURSESID, CREDITSATTEMPTED, CREDITSAWARDED, OTHERCOURSE, EA7SESSIONSID, REPEATALLOWED, ISALTERNATE, STUDENTCOURSELINK, GRADEVALUE, EA7TRANSLATIONSID, TTSEQUENCE, EA7SG_GRADEVALUE, EA7SG_EA7TRANSLATIONENTRIESID, GRADETYPE, CREDITEARNED
	FROM @TEMPSTUDENTCOURSEINFO SCI
	ORDER BY SCI.EA7COURSESID, SCI.CREDITEARNED, coalesce(SCI.CREDITSAWARDED,0) DESC, coalesce(SCI.GRADEVALUE,0) DESC

	DELETE @TEMPSTUDENTCOURSEINFO		

	--CR338421-082211 it's alot to say, if there's a term for this course that needs a grade, don't award credit for the course yet, count it as in progress (until all terms have grades)
	--Setting this after the sort so the order will still take into account the awarded credit
	--CR343639-061212 Change a little for the above, if there's a term for this course that needs a grade and the student also enrolled the class in this term, don't award credit for the course yet, count it as in progress (until all terms have grades). If the student does not enroll the class in this term or withdraws from the class in this term, we will award credit for the course in the previous term(s).
	UPDATE @STUDENTCOURSEINFO 
	SET CREDITSAWARDED = 0, 
		PARTIALCREDIT = dbo.sf_EA7GetAwardedCreditsByEA7StudentCoursesID(sc.EA7STUDENTCOURSESID) 	
	from @STUDENTCOURSEINFO sc
	inner join EA7COURSEGRADINGINFO cgi on sc.EA7COURSESID = cgi.EA7COURSESID AND sc.EA7SESSIONSID = cgi.EA7SESSIONSID
	where exists (select 1 from EA7STUDENTCOURSETERMS
								inner join EA7TERMS on EA7TERMS.EA7SESSIONSID = sc.EA7SESSIONSID and EA7STUDENTCOURSETERMS.EA7TermsID = EA7TERMS.EA7TermsID
								inner join EA7COURSEGRADINGINFOGRADES on EA7COURSEGRADINGINFOGRADES.EA7COURSEGRADINGINFOID = cgi.EA7COURSEGRADINGINFOID
								inner join EA7MARKINGCOLUMNS on EA7COURSEGRADINGINFOGRADES.EA7MARKINGCOLUMNSID = EA7MARKINGCOLUMNS.EA7MARKINGCOLUMNSID
								left join EA7MARKINGCOLUMNTERMS ON EA7MARKINGCOLUMNTERMS.TERMSID = EA7TERMS.TERMID and EA7MARKINGCOLUMNTERMS.EA7MARKINGCOLUMNSID = EA7MARKINGCOLUMNS.EA7MARKINGCOLUMNSID												
								left join EA7STUDENTGRADES on EA7STUDENTGRADES.EA7MARKINGCOLUMNSID = EA7MARKINGCOLUMNS.EA7MARKINGCOLUMNSID and EA7STUDENTGRADES.EA7STUDENTCOURSESID = EA7STUDENTCOURSETERMS.EA7STUDENTCOURSESID
								where EA7COURSEGRADINGINFOGRADES.AWARDCREDITSIN = -1 and
									  EA7STUDENTCOURSETERMS.EA7STUDENTCOURSESID = sc.EA7STUDENTCOURSESID and
									 (EA7MARKINGCOLUMNS.INCLUDESELECTTERMS = 1 or EA7MARKINGCOLUMNTERMS.EA7MARKINGCOLUMNTERMSID is not null) and
									 ((EA7STUDENTGRADES.EA7STUDENTGRADESID is null) or ((coalesce(EA7STUDENTGRADES.CREDITAWARDED, 0) = 0) AND (EA7STUDENTGRADES.GRADEVALUE is null AND EA7STUDENTGRADES.EA7TRANSLATIONENTRIESID is null))) and EA7STUDENTCOURSETERMS.ENROLLMENTSTATUS = 2 and
									 --This terms is after the graded term
									 (EA7TERMS.SEQUENCE > (select coalesce(max(T2.SEQUENCE), EA7TERMS.SEQUENCE) from EA7TERMS T2
													inner join EA7STUDENTCOURSETERMS SCT2 on SCT2.EA7TERMSID = T2.EA7TERMSID and SCT2.EA7STUDENTCOURSESID = sc.EA7STUDENTCOURSESID													
													inner join EA7STUDENTGRADES SG2 on SCT2.EA7STUDENTCOURSESID = SG2.EA7STUDENTCOURSESID and SG2.EA7STUDENTCOURSESID = sc.EA7STUDENTCOURSESID 
													inner join EA7MARKINGCOLUMNS MC2 on SG2.EA7MARKINGCOLUMNSID = MC2.EA7MARKINGCOLUMNSID
													left join EA7MARKINGCOLUMNTERMS MCT2 ON MCT2.EA7MARKINGCOLUMNSID = MC2.EA7MARKINGCOLUMNSID and MCT2.TERMSID = T2.TERMID
													where (coalesce(SG2.CREDITAWARDED, 0) > 0) and
														(MC2.INCLUDESELECTTERMS = 1 or MCT2.EA7MARKINGCOLUMNTERMSID is not null) and
														--CR338421-082211 if the graded term and the ungraded term are withdawn then this course should be counted as complete
														(SCT2.ENROLLMENTSTATUS <> 3 or (SCT2.ENROLLMENTSTATUS = 3 and EA7STUDENTCOURSETERMS.ENROLLMENTSTATUS <> 3)))) and
									--And there is not another graded marking column in this term
									NOT EXISTS (select 1 from EA7STUDENTGRADES SG3 
										inner join EA7STUDENTCOURSETERMS SCT3 ON SCT3.EA7STUDENTCOURSESID = SG3.EA7STUDENTCOURSESID
										inner join EA7MARKINGCOLUMNS MC3 on SG3.EA7MARKINGCOLUMNSID = MC3.EA7MARKINGCOLUMNSID
										left join EA7MARKINGCOLUMNTERMS MCT3 ON MCT3.EA7MARKINGCOLUMNSID = MC3.EA7MARKINGCOLUMNSID and MCT3.TERMSID = EA7TERMS.TERMID
										where SG3.EA7STUDENTCOURSESID = sc.EA7STUDENTCOURSESID and
											SCT3.EA7TERMSID = EA7TERMS.EA7TERMSID and
											SCT3.EA7STUDENTCOURSESID = sc.EA7STUDENTCOURSESID and
											COALESCE(SG3.CREDITAWARDED, 0) > 0 and
											(MC3.INCLUDESELECTTERMS = 1 or MCT3.EA7MARKINGCOLUMNTERMSID is not null)))
																						
	--CR310381-102308 allow other courses to be listed as planned
	/*
	DELETE @STUDENTCOURSEINFO
	FROM @STUDENTCOURSEINFO SCI LEFT OUTER JOIN EA7STUDENTGRADES ON SCI.EA7STUDENTCOURSESID = EA7STUDENTGRADES.EA7STUDENTCOURSESID
	WHERE OTHERCOURSE = -1 AND ea7studentgrades.EA7STUDENTGRADESID IS null
	*/
	
	--we will not consider courses that have received a grade and no credit
	--ScottKa 06/17/2009 7.82 Zero Credit Courses - zero credit courses fullfilling tasks are exempt from this.
	DELETE @STUDENTCOURSEINFO
	FROM @STUDENTCOURSEINFO SCI
	WHERE (EA7STUDENTCOURSESID IN 
		(SELECT SG.ea7studentcoursesid
		FROM ea7studentgrades SG 
		inner join @STUDENTCOURSEINFO SCI2 on SG.ea7studentcoursesid = SCI2.EA7STUDENTCOURSESID
		inner join ea7coursegradinginfogrades CGIG on SG.ea7coursegradinginfogradesid = CGIG.ea7coursegradinginfogradesid
		WHERE CGIG.AWARDCREDITSIN = -1 AND (ea7translationentriesid IS NOT NULL OR SG.GRADEVALUE IS NOT NULL)
		GROUP BY SG.ea7studentcoursesid
		HAVING (coalesce(sum(CREDITAWARDED),0) = 0)))
		AND (EA7STUDENTCOURSESID NOT IN 
		(SELECT SCI2.ea7studentcoursesid
		FROM @STUDENTCOURSEINFO SCI2
		inner join EA7DEGREEREQCOURSES DRC on SCI2.EA7COURSESID = DRC.EA7COURSESID
		inner join EA7DEGREEREQUIREMENTS DR on DRC.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID
		inner join EA7STUDENTDEGREEREQS SDR ON DR.EA7DEGREEREQUIREMENTSID = SDR.EA7DEGREEREQUIREMENTSID
		inner join EA7STUDENTDEGREES SD ON SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID
		WHERE (DR.COMPLETEDUSING = 2) AND SD.ISWHATIF = @bIsWhatIf))
						
				
--CR315526-022309 instances not awarding credits should be considered as additional courses; unless none of the instances award credit				
--CR315059-021109 commented out code block below and modified it so that each course with multiple intances is considered; before it was only returning the top 1 of all courses
--need to remove extra instances of courses that cannot be awarded credit multiple times	
	DELETE @STUDENTCOURSEINFO
	FROM @STUDENTCOURSEINFO SCI4
	INNER JOIN (
		SELECT EA7COURSESID	
		FROM @STUDENTCOURSEINFO SCI2
		WHERE RepeatAllowed = 0
		GROUP BY EA7COURSESID
		HAVING COUNT(EA7COURSESID) > 1)	SCI3 ON SCI3.EA7COURSESID = SCI4.EA7COURSESID
	WHERE SCI4.RepeatAllowed = 0
	AND ((SCI4.EA7STUDENTCOURSESID NOT IN 
		(SELECT TOP 1 EA7STUDENTCOURSESID 		
		 FROM @STUDENTCOURSEINFO SCI
		 WHERE SCI.EA7COURSESID = SCI3.EA7COURSESID AND COALESCE(SCI.CREDITSAWARDED, 0) = 0
		 ORDER BY SCI.EA7COURSESID, coalesce(SCI.CREDITSAWARDED,0) DESC, coalesce(SCI.GRADEVALUE,0) DESC) 
		 )	
		AND
			NOT EXISTS	
					(SELECT EA7STUDENTCOURSESID 		
					 FROM @STUDENTCOURSEINFO SCI
					 WHERE SCI.EA7COURSESID = SCI3.EA7COURSESID AND COALESCE(SCI.CREDITSAWARDED, 0) > 0))
	OR ((SCI4.EA7STUDENTCOURSESID NOT IN 
		(SELECT EA7STUDENTCOURSESID 		
		 FROM @STUDENTCOURSEINFO SCI
		 WHERE SCI.EA7COURSESID = SCI3.EA7COURSESID AND COALESCE(SCI.CREDITSAWARDED, 0) > 0) 
		 )	
		AND
			 EXISTS	
					(SELECT EA7STUDENTCOURSESID 		
					 FROM @STUDENTCOURSEINFO SCI
					 WHERE SCI.EA7COURSESID = SCI3.EA7COURSESID AND COALESCE(SCI.CREDITSAWARDED, 0) > 0))
	
/*	DELETE @STUDENTCOURSEINFO
	FROM @STUDENTCOURSEINFO SCI4
	WHERE SCI4.RepeatAllowed = 0
	AND SCI4.EA7STUDENTCOURSESID NOT IN 
		(SELECT EA7STUDENTCOURSESID  --CR308138-091108 removed top 1 
		 FROM @STUDENTCOURSEINFO SCI
		 WHERE SCI.EA7COURSESID IN (
			SELECT EA7COURSESID	
			FROM @STUDENTCOURSEINFO SCI2
			WHERE RepeatAllowed = 0
			GROUP BY EA7COURSESID
			HAVING COUNT(EA7COURSESID) > 1)
		--CR308138-091108 moving the order by above to where the data was inserted into the temp table
		--04/09/2007 SAM CR271534-033007 see notes at beginning of procedure
		--ORDER BY SCI.EA7COURSESID, coalesce(SCI.CREDITSAWARDED,0) DESC, coalesce(SCI.GRADEVALUE,0) DESC
		)
	AND SCI4.EA7COURSESID IN (
		SELECT EA7COURSESID	
		FROM @STUDENTCOURSEINFO SCI2
		WHERE RepeatAllowed = 0
		GROUP BY EA7COURSESID
		HAVING COUNT(EA7COURSESID) > 1)
*/	

	--CR255302-092006 - need to remove unused alternates if main is enrolled, or unused main if alternate is enrolled
	DELETE @STUDENTCOURSEINFO
	FROM @STUDENTCOURSEINFO SCI 
	INNER JOIN EA7STUDENTCOURSES MAIN ON SCI.EA7STUDENTCOURSESID = MAIN.EA7STUDENTCOURSESID
	LEFT OUTER JOIN EA7STUDENTCOURSES ALT ON MAIN.EA7STUDENTCOURSESID = ALT.STUDENTCOURSELINK
	LEFT OUTER JOIN EA7STUDENTCOURSES PARENT ON MAIN.STUDENTCOURSELINK = PARENT.EA7STUDENTCOURSESID
	--CR313684-011209 JamieEl added ISNULL
	WHERE main.EA7STUDENTSID=@EA7STUDENTSID AND ((ISNULL(MAIN.ENROLLEDTERMS,0) = 0 AND ISNULL(ALT.ENROLLEDTERMS,0) > 0) OR (ISNULL(MAIN.ENROLLEDTERMS,0) = 0 AND ISNULL(PARENT.ENROLLEDTERMS,0) > 0))

	--need to remove courses that have a grading record, but no marking columns have been selected to be graded
	DELETE @STUDENTCOURSEINFO
	FROM @STUDENTCOURSEINFO SCI 
	INNER JOIN EA7STUDENTCOURSES SC ON SCI.EA7STUDENTCOURSESID = SC.EA7STUDENTCOURSESID
	INNER JOIN EA7COURSES C ON SC.EA7COURSESID = C.EA7COURSESID
	INNER JOIN EA7COURSEGRADINGINFO CGI ON C.EA7COURSESID = CGI.EA7COURSESID AND SC.EA7SESSIONSID = CGI.EA7SESSIONSID
	LEFT OUTER JOIN EA7COURSEGRADINGINFOGRADES CGIG ON CGI.EA7COURSEGRADINGINFOID = CGIG.EA7COURSEGRADINGINFOID AND CGIG.USEMARKINGCOLUMN=-1
	WHERE SC.ISOTHERCOURSE = 0 AND EA7COURSEGRADINGINFOGRADESID IS NULL
	
	--need to remove courses that have a grading record, and have marking columns selected for grading, but no marking columns are
	--selected to award credit in...
	--note: cannot include this in statement above by putting usemarkingcolumn and awardcreditsin in the same join because
	--	this would wrongly eliminate instances where credit is awarded in a different marking column than the grade is earned
	--ScottKa 06/17/2009 7.82 Zero Credit Courses - zero credit courses fullfilling tasks are exempt from this.
	DELETE @STUDENTCOURSEINFO
	FROM @STUDENTCOURSEINFO SCI 
	INNER JOIN EA7STUDENTCOURSES SC ON SCI.EA7STUDENTCOURSESID = SC.EA7STUDENTCOURSESID
	INNER JOIN EA7COURSES C ON SC.EA7COURSESID = C.EA7COURSESID
	INNER JOIN EA7COURSEGRADINGINFO CGI ON C.EA7COURSESID = CGI.EA7COURSESID AND SC.EA7SESSIONSID = CGI.EA7SESSIONSID
	LEFT OUTER JOIN EA7COURSEGRADINGINFOGRADES CGIG ON CGI.EA7COURSEGRADINGINFOID = CGIG.EA7COURSEGRADINGINFOID AND CGIG.AWARDCREDITSIN=-1
	WHERE (SC.ISOTHERCOURSE = 0 AND EA7COURSEGRADINGINFOGRADESID IS NULL)
	AND (SCI.EA7STUDENTCOURSESID NOT IN 
		(SELECT SCI2.ea7studentcoursesid
		FROM @STUDENTCOURSEINFO SCI2
		inner join EA7DEGREEREQCOURSES DRC on SCI2.EA7COURSESID = DRC.EA7COURSESID
		inner join EA7DEGREEREQUIREMENTS DR on DRC.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID
		inner join EA7STUDENTDEGREEREQS SDR ON DR.EA7DEGREEREQUIREMENTSID = SDR.EA7DEGREEREQUIREMENTSID
		inner join EA7STUDENTDEGREES SD ON SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID
		WHERE (DR.COMPLETEDUSING = 2) AND SD.ISWHATIF = @bIsWhatIf))

	--CR265009-012907 SAM - remove global alternates that are not enrolled, but every other request for that session is (or its alternate)
	DELETE @STUDENTCOURSEINFO
	FROM @STUDENTCOURSEINFO sc1
	LEFT OUTER JOIN EA7STUDENTCOURSETERMS SCT ON sc1.EA7STUDENTCOURSESID = SCT.EA7STUDENTCOURSESID
	WHERE  ISALTERNATE = -1 
	AND coalesce(STUDENTCOURSELINK,0) = 0
	AND (coalesce(SCT.ENROLLMENTSTATUS,0) = 1 OR SCT.ENROLLMENTSTATUS IS NULL)
	AND NOT EXISTS (
		SELECT sc2.ea7studentcoursesid
		FROM EA7STUDENTCOURSES sc2
		LEFT OUTER JOIN EA7STUDENTCOURSES sc2_alternate ON sc2.ea7studentcoursesid = sc2_alternate.STUDENTCOURSELINK
		LEFT OUTER JOIN EA7STUDENTCOURSES sc2_link ON sc2.STUDENTCOURSELINK = sc2_link.ea7studentcoursesid
		WHERE ((coalesce(sc2.STUDENTCOURSELINK,0) > 0 AND sc2.ISALTERNATE =-1) OR sc2.ISALTERNATE = 0)
		AND sc2.EA7SESSIONSID = sc1.EA7SESSIONSID
		AND (sc2.ENROLLEDTERMS = 0 AND coalesce(sc2_alternate.ENROLLEDTERMS,0)=0 AND coalesce(sc2_link.ENROLLEDTERMS,0)=0)
		AND sc2.EA7STUDENTSID = @EA7STUDENTSID)

	--remove courses that are used in substitutions and do not have Use In Other Requirements selected
	DELETE @STUDENTCOURSEINFO
	FROM @STUDENTCOURSEINFO sc1
	INNER JOIN EA7STUDENTDEGREEREQSUBS SDRS On sc1.EA7STUDENTCOURSESID = SDRS.EA7STUDENTCOURSESID
	INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDRS.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
	INNER JOIN EA7STUDENTDEGREES SD ON SDR.EA7STUDENTDEGREESID = SDR.EA7STUDENTDEGREESID
	WHERE SDRS.USEINOTHERREQUIREMENTS = 0 AND SD.ISWHATIF = @bIsWhatIf

	UPDATE @STUDENTDEGREEINFO
	SET HIGHESTGRADE = (select MAX(SCI.GRADEVALUE) 
				FROM @STUDENTCOURSEINFO SCI 
				WHERE SDI.EA7COURSESID = SCI.EA7COURSESID)
	FROM @STUDENTDEGREEINFO SDI
	
	--CR323949-082509,CR323614-081309,CR323468-081009 If the course was already used in a completed degree and Can Satisfy was marked as "Only this req" or "Only this degree", remove it from the temp table because it cannot be used again
	DELETE @STUDENTCOURSEINFO from @STUDENTCOURSEINFO SCI where SCI.EA7COURSESID in (
	select SDRC.EA7COURSESID from EA7STUDENTDEGREEREQCOURSES SDRC
	inner join EA7STUDENTDEGREEREQS SDR on SDR.EA7STUDENTDEGREEREQSID =SDRC.EA7STUDENTDEGREEREQSID
	inner join EA7STUDENTDEGREES SD on SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID
	inner join EA7DEGREEREQCOURSES DRC on SDRC.EA7DEGREEREQCOURSESID = DRC.EA7DEGREEREQCOURSESID
	where SD.EA7STUDENTSID = @EA7STUDENTSID and SD.STATUS = 2 and DRC.CANSATISFY in (2,4))
	
	--CR318270-041709
	UPDATE @STUDENTDEGREEINFO
	SET CREDITSAWARDED = (select COALESCE(SUM(SCI.CREDITSAWARDED), 0)
				FROM @STUDENTCOURSEINFO SCI 
				WHERE SDI.EA7COURSESID = SCI.EA7COURSESID)
	FROM @STUDENTDEGREEINFO SDI

	--CR338421-082211 added partial credit to use when sorting requirements
	UPDATE @STUDENTDEGREEINFO
	SET PARTIALCREDIT = (select COALESCE(SUM(SCI.PARTIALCREDIT), 0)
				FROM @STUDENTCOURSEINFO SCI 
				WHERE SDI.EA7COURSESID = SCI.EA7COURSESID)
	FROM @STUDENTDEGREEINFO SDI
	--This update statement attempts to calculate the number of credits possible for an entire requirement based
	--on the number of credits attempted from the course grading info record.  The correct course grading info 
	--record should be selected based on the session id from the student course record, or if no student course record
	--exists the most recent one as of the system date should be used (ignoring any ones in the future)
	
	--JAY CR271422-032907 04/17/07
	--Changing this as suggested by Kirby for these reasons:
	-- 1. 1/7 the number of reads
	-- 2. About 1/4 the cpu time
	-- 3. 2 to 3 times faster
	-- 4. It works the way the comment above the statement says.  What's currently there doesn't

	UPDATE @StudentDegreeInfo
	SET CreditsPossible = ISNULL(CGI.TotalAttemptedCredits, CGI2.TotalAttemptedCredits)
	FROM @StudentDegreeInfo SDI left outer JOIN @StudentCourseInfo SC ON SDI.EA7CoursesID = SC.EA7CoursesID
	LEFT JOIN EA7CourseGradingInfo CGI ON SDI.EA7CoursesID = CGI.EA7CoursesID AND CGI.EA7SessionsID = SC.EA7SessionsID 
	INNER JOIN EA7CourseGradingInfo CGI2 ON SDI.EA7CoursesID = CGI2.EA7CoursesID 
	INNER JOIN 
		(SELECT MAX(A2.EA7SessionsID) AS EA7SessionsID, A1.EA7CoursesID
		FROM EA7CourseGradingInfo A1 INNER JOIN EA7Terms A2 ON A1.EA7SessionsID = A2.EA7SessionsID
		INNER JOIN	
			(SELECT MAX(StartDate) AS Start_Date, t1.EA7CoursesID 
			FROM @STUDENTDEGREEINFO t1 INNER JOIN EA7CourseGradingInfo t2 ON t1.EA7CoursesID = t2.EA7CoursesID
			INNER JOIN EA7Terms t3 on t2.EA7SessionsID = t3.EA7SessionsID
			WHERE StartDate <= GetDate()
			GROUP BY t1.EA7CoursesID) V1
		ON A2.StartDate = V1.Start_Date AND A1.EA7CoursesID = V1.EA7CoursesID
		GROUP BY A1.EA7CoursesID) V2
	ON CGI2.EA7CoursesID = V2.EA7CoursesID AND CGI2.EA7SessionsID = V2.EA7SessionsID

	UPDATE @STUDENTDEGREEINFO
	SET CREDITSSUMBYREQUIREMENT = 
		(SELECT SUM(CREDITSPOSSIBLE) 
		FROM @STUDENTDEGREEINFO SDI2 
		WHERE SDI.EA7STUDENTDEGREEREQSID = SDI2.EA7STUDENTDEGREEREQSID)
	FROM @STUDENTDEGREEINFO SDI

	--eliminate any issuse caused by a null value here
	UPDATE @STUDENTDEGREEINFO
	SET CREDITSPOSSIBLE = 0
	WHERE CREDITSPOSSIBLE IS NULL
	
	--find the total amount exempted/substituted
	INSERT INTO @ExemptedSubstitutedTotals (EA7STUDENTDEGREESID, TotalExemptedSubstitutedCredits, PROCESSED)
	select SD.EA7StudentDegreesID, dbo.sf_EA7GETTotalExemptedCreditsForDegree(sd.ea7studentdegreesid) + coalesce(sum(case SDRS.USEINOTHERREQUIREMENTS when -1 then SDRS.CREDITSUSED else 0 end),0), 0
	FROM EA7STUDENTDEGREEREQS SDR 
	INNER JOIN EA7STUDENTDEGREES SD ON SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID
	LEFT OUTER JOIN EA7StudentDegreeReqSubs SDRS on SDR.EA7STUDENTDEGREEREQSID = SDRS.EA7STUDENTDEGREEREQSID
	WHERE SD.EA7STUDENTSID = @EA7STUDENTSID AND SD.COMPLETEDON IS NULL AND SD.ISWHATIF = @bIsWhatIf
	GROUP BY SD.EA7STUDENTDEGREESID,SD.PARENTDEGREEID,SD.DEGREETYPE
	
	--fill table with info necessary for distributing exempted/substituted credits
	INSERT INTO @CalculateExemptionsSubstitutions(EA7STUDENTDEGREEREQSID, CREDITSREQUIRED, CREDITSREQUIREDTO, DIFF, EA7STUDENTDEGREESID)
	SELECT ea7studentdegreereqsid, creditsrequired, creditsrequiredto, (CREDITSREQUIREDTO - CREDITSREQUIRED), EA7STUDENTDEGREESID
	FROM @StudentDegreeInfo SDI
	WHERE SDI.GENERALELECTIVE = -1
	GROUP BY ea7studentdegreereqsID, Creditsrequired, creditsrequiredto, CREDITSSUMBYREQUIREMENT,DEGREE_PRIORITY,EA7STUDENTDEGREESID,REQUIREMENT_PRIORITY
	ORDER BY DEGREE_PRIORITY, EA7STUDENTDEGREESID, REQUIREMENT_PRIORITY

	--calculate running total by row for the amount of exempted/substituted credits allowed based on difference
	UPDATE @CalculateExemptionsSubstitutions
	SET PREVIOUSTOTAL =  
		(SELECT coalesce(SUM(DIFF),0) 
		FROM @CalculateExemptionsSubstitutions CES2 
		WHERE CES2.CalculateExemptionsSubstitutionsID < CES.CalculateExemptionsSubstitutionsID AND CES.EA7STUDENTDEGREESID = CES2.EA7STUDENTDEGREESID)
	FROM @CalculateExemptionsSubstitutions CES
	INNER JOIN @ExemptedSubstitutedTotals EST ON CES.EA7STUDENTDEGREESID = EST.EA7STUDENTDEGREESID

	UPDATE @CalculateExemptionsSubstitutions
	SET CREDITSREQUIRED =
		CASE
			WHEN ((EST.TOTALEXEMPTEDSUBSTITUTEDCREDITS - CES.PREVIOUSTOTAL) > 0) THEN
				CASE 
					WHEN (EST.TOTALEXEMPTEDSUBSTITUTEDCREDITS - CES.PreviousTotal) > CES.DIFF THEN CES.CREDITSREQUIREDTO
					ELSE (EST.TOTALEXEMPTEDSUBSTITUTEDCREDITS - CES.PREVIOUSTOTAL) + CES.CREDITSREQUIRED
				END
			ELSE CREDITSREQUIRED
		END
	FROM @CalculateExemptionsSubstitutions CES
	INNER JOIN @ExemptedSubstitutedTotals EST ON CES.EA7STUDENTDEGREESID = EST.EA7STUDENTDEGREESID

	--keep track of unapplied exempted/substituted credits so if we remove extra credits from general electives due to group functionality we can apply the unapplied exempted/substituted credits then
	UPDATE @ExemptedSubstitutedTotals
	SET TotalUnappliedCredits = (select 
		CASE
			WHEN EST.TOTALEXEMPTEDSUBSTITUTEDCREDITS > (CES.PREVIOUSTOTAL + CES.DIFF) THEN EST.TOTALEXEMPTEDSUBSTITUTEDCREDITS - (CES.PREVIOUSTOTAL + CES.DIFF)
			ELSE 0
		END
		FROM @CalculateExemptionsSubstitutions CES
		WHERE CES.CalculateExemptionsSubstitutionsID IN 
			(SELECT TOP 1 CES2.CalculateExemptionsSubstitutionsID FROM @CalculateExemptionsSubstitutions CES2 
			 WHERE CES2.EA7STUDENTDEGREESID = EST.EA7STUDENTDEGREESID 
			ORDER BY CES2.PREVIOUSTOTAL DESC))
	FROM  @ExemptedSubstitutedTotals EST

	UPDATE @StudentDegreeInfo
	SET CREDITSREQUIRED = CES.CREDITSREQUIRED,
	CREDITSREQUIREDTO = CES.CREDITSREQUIREDTO
	FROM @StudentDegreeInfo SDI 
	INNER JOIN @CalculateExemptionsSubstitutions CES ON SDI.EA7STUDENTDEGREEREQSID = CES.EA7STUDENTDEGREEREQSID

	--update DB tables also...
	UPDATE EA7STUDENTDEGREEREQS
	SET CREDITSREQUIRED = CES.CREDITSREQUIRED,
	CREDITSREQUIREDTO = CES.CREDITSREQUIREDTO
	FROM EA7STUDENTDEGREEREQS SDR
	INNER JOIN @CalculateExemptionsSubstitutions CES ON SDR.EA7STUDENTDEGREEREQSID = CES.EA7STUDENTDEGREEREQSID

	--update group info in DB in case it has been changed in config
	UPDATE EA7STUDENTDEGREEREQS
	SET NUMBERGROUPSDEFINED = CASE DR.MINIMUMCREDITSFORGROUPTYPE WHEN 1 THEN (SELECT COUNT(DISTINCT GROUPID) FROM EA7DEGREEREQCOURSES DRC WHERE DRC.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID)
				  ELSE (SELECT COUNT(DISTINCT GROUPID) FROM EA7DEGREEREQGROUPS DRG WHERE DRG.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID) END,
	NUMBERGROUPSREQUIRED = SDI.NUMBERGROUPSREQUIRED,
	GROUPCREDITSTYPE = DR.MINIMUMCREDITSFORGROUPTYPE
	FROM @STUDENTDEGREEINFO SDI
	INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDI.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
	INNER JOIN EA7DEGREEREQUIREMENTS DR ON SDR.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID

	--fill in group info
	INSERT INTO EA7STUDENTDEGREEREQGROUPS(EA7STUDENTDEGREEREQSID, GROUPID, CREDITSREQUIRED, WAIVEDCREDITS, EXEMPTEDCREDITS, SUBSTITUTEDCREDITs, STATUS, INCLUDED)
	SELECT DISTINCT SDR.EA7STUDENTDEGREEREQSID,
	DRC.GROUPID,
	CASE SDR.GROUPCREDITSTYPE
		WHEN 1 THEN DR.MINIMUMCREDITSFORGROUP
		ELSE (SELECT DRG.CREDITSREQUIRED FROM EA7DEGREEREQGROUPS DRG 
			WHERE DRG.GROUPID = DRC.GROUPID 
			AND DRG.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID)
	END,
	(SELECT COALESCE(WEG1.CREDITS,0) FROM EA7STUDENTDEGREEREQWAIVEEXEMPTGROUPS WEG1 WHERE WEG1.WAIVEEXEMPTTYPE = 1 AND WEG1.GROUPID = DRC.GROUPID AND WEG1.EA7STUDENTDEGREEREQSID = SDI.EA7STUDENTDEGREEREQSID),	
	(SELECT COALESCE(WEG2.CREDITS,0) FROM EA7STUDENTDEGREEREQWAIVEEXEMPTGROUPS WEG2 WHERE WEG2.WAIVEEXEMPTTYPE = 2 AND WEG2.GROUPID = DRC.GROUPID AND WEG2.EA7STUDENTDEGREEREQSID = SDI.EA7STUDENTDEGREEREQSID),
	(SELECT COALESCE(SUM(SDRS.CREDITSUSED),0) FROM EA7STUDENTDEGREEREQSUBS SDRS WHERE SDRS.INGROUPID = DRC.GROUPID AND SDRS.EA7STUDENTDEGREEREQSID = SDI.EA7STUDENTDEGREEREQSID),
	1, -1
	FROM @STUDENTDEGREEINFO SDI
	INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDI.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
	INNER JOIN EA7DEGREEREQUIREMENTS DR ON SDR.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID
	INNER JOIN EA7DEGREEREQCOURSES DRC ON DR.EA7DEGREEREQUIREMENTSID = DRC.EA7DEGREEREQUIREMENTSID
	WHERE DRC.GROUPID IS NOT NULL

	--update group status
	UPDATE EA7STUDENTDEGREEREQGROUPS
	SET STATUS = 2
	FROM EA7STUDENTDEGREEREQGROUPS SDRG
	INNER JOIN @STUDENTDEGREEINFO SDI ON SDRG.EA7STUDENTDEGREEREQSID = SDI.EA7STUDENTDEGREEREQSID
	WHERE coalesce(coalesce(SDRG.WAIVEDCREDITS,0) + coalesce(SDRG.SUBSTITUTEDCREDITS,0) + coalesce(SDRG.EXEMPTEDCREDITS,0) ,0) >= coalesce(SDRG.CREDITSREQUIRED,0)

	--set the included status of groups that are made invalid by groups in the same req that have more credits due to exempt/waive/substitute credits to 0
	UPDATE EA7STUDENTDEGREEREQGROUPS
	SET INCLUDED = 0
	FROM EA7STUDENTDEGREEREQGROUPS SDRG2
	INNER JOIN @STUDENTDEGREEINFO SDI ON SDRG2.EA7STUDENTDEGREEREQSID = SDI.EA7STUDENTDEGREEREQSID
	WHERE EA7STUDENTDEGREEREQGROUPSID IN
		(SELECT DISTINCT SDRG3.EA7STUDENTDEGREEREQGROUPSID
		 FROM EA7STUDENTDEGREEREQGROUPS SDRG
		 INNER JOIN @STUDENTDEGREEINFO SDI ON SDRG.EA7STUDENTDEGREEREQSID = SDI.EA7STUDENTDEGREEREQSID
		  INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDI.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
		 INNER JOIN EA7DEGREEREQGROUPS DRG ON SDR.EA7DEGREEREQUIREMENTSID = DRG.EA7DEGREEREQUIREMENTSID
		 INNER JOIN FILTERVALUES7 ON DRG.EA7DEGREEREQGROUPSID = FILTERVALUES7.PARENTID
		 INNER JOIN EA7STUDENTDEGREEREQGROUPS SDRG3 ON FILTERVALUES7.FILTERIDVALUE1 = SDRG2.GROUPID AND SDRG3.EA7STUDENTDEGREEREQSID = SDRG.EA7STUDENTDEGREEREQSID
		 WHERE FILTERVALUES7.INCLUDEOPTION = 2 AND FILTERVALUES7.RECORDTYPE = 1461 AND SDRG.EA7STUDENTDEGREEREQSID = SDRG2.EA7STUDENTDEGREEREQSID
		 GROUP BY SDRG3.EA7STUDENTDEGREEREQGROUPSID, SDRG.WAIVEDCREDITS, SDRG.EXEMPTEDCREDITs, SDRG.SUBSTITUTEDCREDITS
		 HAVING ((coalesce(SDRG.WAIVEDCREDITS,0) + coalesce(SDRG.EXEMPTEDCREDITS,0) + coalesce(SDRG.SUBSTITUTEDCREDITS,0)) > (coalesce(SDRG2.WAIVEDCREDITS,0) + coalesce(SDRG2.EXEMPTEDCREDITS,0) + coalesce(SDRG2.SUBSTITUTEDCREDITS,0))))

	--update requirements that are complete due to substitutions/waives/exemptions
	UPDATE EA7STUDENTDEGREEREQS
	SET STATUS = 2
	FROM EA7STUDENTDEGREEREQS SDR
	INNER JOIN @STUDENTDEGREEINFO SDI ON SDR.EA7STUDENTDEGREEREQSID = SDI.EA7STUDENTDEGREEREQSID --CR304778-071608
	INNER JOIN EA7DEGREEREQUIREMENTS DR ON SDR.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID
	WHERE (DR.REQUIREMENTFULFILLEDTYPE = 1 OR DR.NUMBEROFGROUPS = 1 OR DR.NUMBEROFGROUPS = (Select Count(*) FROM EA7STUDENTDEGREEREQGROUPS SDRG3 WHERE SDRG3.STATUS = 2 AND SDRG3.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID AND SDRG3.INCLUDED = -1))
	AND (((coalesce(SDR.WAIVEDCREDITS,0) + coalesce(SDR.EXEMPTEDCREDITS,0) + coalesce(SDR.SUBSTITUTEDCREDITS,0) - (SELECT coalesce(sum(SDRG.WAIVEDCREDITS),0) + coalesce(sum(SDRG.EXEMPTEDCREDITS),0) + coalesce(sum(SDRG.SUBSTITUTEDCREDITS),0)
		FROM EA7STUDENTDEGREEREQGROUPS SDRG
		WHERE SDRG.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID  AND ((SDRG.INCLUDED = 0) OR (SDRG.INCLUDED = -1 AND SDRG.STATUS <> 2)))) >= SDR.CREDITSREQUIRED AND DR.COMPLETEDUSING = 1) OR (DR.COMPLETEDUSING = 2 AND SDR.WAIVED = -1))
		
	--for requirements with a substitution where forcourse is specified remove that course requirement from the studentdegreeinfo table
	DELETE FROM @STUDENTDEGREEINFO WHERE EA7DEGREEREQCOURSESID IN 
		(SELECT DRC.EA7DEGREEREQCOURSESID
		 FROM EA7STUDENTDEGREEREQSUBS SDRS
		 INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDRS.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
		 INNER JOIN EA7DEGREEREQCOURSES DRC ON SDR.EA7DEGREEREQUIREMENTSID = DRC.EA7DEGREEREQUIREMENTSID AND DRC.EA7COURSESID = SDRS.FORCOURSEID
		 INNER JOIN EA7STUDENTDEGREES SD ON SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID
		 WHERE SDRS.FORCOURSEID IS NOT NULL AND COALESCE(SDRS.CREDITSUSED,0) > 0 AND SD.EA7STUDENTSID = @EA7STUDENTSID AND SD.COMPLETEDON IS NULL AND SD.ISWHATIF = @bIsWhatIf)

	declare @STUDENTDEGREEINFOID int, @EA7STUDENTDEGREESID int, @EA7STUDENTDEGREEREQSID int,
		@EA7DEGREEREQCOURSESID int, @EA7COURSESID int, @CREDITSREQUIRED numeric(19,4),
		@CREDITSREQUIREDTO numeric(19,4), @CANSATISFY smallint, @DEGREE_PRIORITY int,
		@REQUIREMENT_PRIORITY int, @CREDITSSUMBYREQUIREMENT numeric(19,4), @STATUS smallint,
		@REQCOURSESID int, @CREDITSUSED numeric(19,4), @OtherCreditsUsed numeric(19,4), @EA7DEGREESID int,
		@MaxOtherCredits numeric(19,4), @CurrentOtherCredits numeric(19,4), @CurrentCredits numeric(19,4),
		@CreditsToUse numeric(19,4), @GroupID int, @NumberGroupsRequired int, @NumberGroupsComplete int,
		@ExtraCredits numeric(19,4), @UnappliedExemptSubstituteCredits numeric(19,4), @GECreditsRequired numeric(19,4),
		@GEReqID int, @ExtraCourseID int, @NonUsableExemptWaiveCredits numeric(19,4),  @CreditsNeeded numeric(19,4),
		@CurrentGroupTotal numeric(19,4), @NewGroupTotal numeric(19,4), @GroupCreditsLeft numeric(19,4), @numOtherCourses int,
		@NumRequiredTasks int, @NumCompletedTasks int, @MinimumGradeRequired smallint, @MinimumGradeValue numeric(19,7), @MinimumGradeTE varchar(10),
		@PARENTSTUDDEGREEID INT
	
	--SAM CR262099-122006 - declare table that will maintain order for the loop, identity column ensures that records will be in the same order they are inserted.
	DECLARE @TempStudentDegTable TABLE (tempid int identity(1,1), STUDENTDEGREEINFOID int, PROCESSED smallint)
	
	--SAM CR262099-122006 - Set order before we loop
	--GPB CR318270-041709 - Added CREDITSAWARDED for the Order by. See notes above.
	--MaryRu CR335708-041311 To use the order that we said was important in CR295631-031108, I added DRCSequence field. 
	--    I just tacked that onto the end since I'm not yet sure exactly how all the other fields we are ordering by are used.
	--CR338421-082211 added partial credit to use when sorting requirements
	--CR341574-022112 only sort by partial credit when no credits are awarded for all instances of a course
	INSERT INTO @TempStudentDegTable(STUDENTDEGREEINFOID, PROCESSED)
	SELECT STUDENTDEGREEINFOID, 0
	FROM @STUDENTDEGREEINFO
	ORDER BY DEGREE_PRIORITY,EA7STUDENTDEGREESID,REQUIREMENT_PRIORITY ASC, 
	HIGHESTGRADE DESC,CREDITSAWARDED DESC, case when creditsawarded = 0 then partialcredit else 0 end desc, EA7STUDENTDEGREEREQSID, DRCSEQUENCE asc

	--don't process complete requirements
	UPDATE @TempStudentDegTable
	SET PROCESSED = -1
	FROM @TempStudentDegTable TSDT
	INNER JOIN @STUDENTDEGREEINFO SDI ON TSDT.STUDENTDEGREEINFOID = SDI.STUDENTDEGREEINFOID
	INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDI.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
	WHERE SDR.STATUS = 2

	--05/14/2007 SAM CR274460-050307 Declare table to keep track of courses taken multiple time
	declare @MultiCourses table
	(
		MultiID int identity(1,1),
		ea7studentcoursesid int,
		credits numeric(19,4),
		RunningTotal numeric(19,4)
	)

	--table to store courses that are not in a completed group but are used to complete a req
	declare @ExtraCourses table
	(
		ExtraCoursesID int identity(1,1),
		EA7STUDENTDEGREEREQCOURSESID int
	)
	
	--loop through course requirements
	WHILE EXISTS(SELECT STUDENTDEGREEINFOID from @TempStudentDegTable where PROCESSED = 0)
	BEGIN	
		--CR341419-021012 reset variable for accurate deletion of tempcourselinks
		set @REQCOURSESID = NULL
		
		--SAM CR262099-122006 - grab first record in our ordered table variable
		SELECT TOP 1 @STUDENTDEGREEINFOID = STUDENTDEGREEINFOID
		FROM @TempStudentDegTable
		where PROCESSED = 0
		
		--SAM CR262099-122006 - pull information associated with the record we grabbed in previous statement
		SELECT TOP 1 @STUDENTDEGREEINFOID=STUDENTDEGREEINFOID, @EA7STUDENTDEGREESID=EA7STUDENTDEGREESID,
			@EA7STUDENTDEGREEREQSID=EA7STUDENTDEGREEREQSID, @EA7DEGREEREQCOURSESID=EA7DEGREEREQCOURSESID,
			@EA7COURSESID=EA7COURSESID, @CREDITSREQUIRED=CREDITSREQUIRED, @CREDITSREQUIREDTO=CREDITSREQUIREDTO,
			@CANSATISFY=CANSATISFY, @DEGREE_PRIORITY=DEGREE_PRIORITY, @REQUIREMENT_PRIORITY=REQUIREMENT_PRIORITY,
			@CREDITSSUMBYREQUIREMENT=CREDITSSUMBYREQUIREMENT, @EA7DEGREESID = EA7DEGREESID, @GroupID = GROUPID,
			@NumberGroupsRequired = NUMBERGROUPSREQUIRED,@MinimumGradeRequired=REQUIREMINIMUMGRADE,
			@MinimumGradeValue = MINIMUMGRADEVALUE,@MinimumGradeTE = MINIMUMGRADE_TE
		FROM @STUDENTDEGREEINFO
		WHERE STUDENTDEGREEINFOID = @STUDENTDEGREEINFOID
		
		Set @OtherCreditsUsed = 0
		
		--CR319525-051409 set parent degree because we need to count other credits used by this degree
		Set @PARENTSTUDDEGREEID = (SELECT CASE SD.DEGREETYPE WHEN 1 THEN SD.EA7STUDENTDEGREESID WHEN 2 THEN SD.PARENTDEGREEID WHEN 3 THEN SD.PARENTDEGREEID WHEN 4 THEN SD2.PARENTDEGREEID WHEN 5 THEN SD2.PARENTDEGREEID END
		FROM EA7STUDENTDEGREES SD
		LEFT JOIN EA7STUDENTDEGREES SD2 ON SD.PARENTDEGREEID = SD2.EA7STUDENTDEGREESID
		WHERE SD.EA7STUDENTDEGREESID = @EA7STUDENTDEGREESID) 		

		--CR319525-051409 count other credits used, by degree
		--CR315304-021809 - first issue - other course credits are combined with normal credits since CR308138-091108 and are getting counted towards othercreditsused
		--get the number of credits from "other" courses
		select @OtherCreditsUsed = coalesce(sum(sdrc2.othercoursecredits),0) from
		--FROM EA7STUDENTDEGREEREQCOURSES sdrc2
		--WHERE EA7STUDENTDEGREEREQCOURSESID IN
		(select (sdrc.creditsused - (case when coalesce(scisum.creditssum, 0) > sdrc.creditsused then 0 else coalesce(scisum.creditssum, 0) end ))'othercoursecredits'
		FROM EA7STUDENTDEGREQCOURSELINKS sdrcl 
		inner join EA7STUDENTDEGREEREQCOURSES SDRC ON sdrcl.EA7STUDENTDEGREEREQCOURSESID = SDRC.EA7STUDENTDEGREEREQCOURSESID
		INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDRC.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
		INNER JOIN EA7STUDENTDEGREES SD ON SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID
		INNER JOIN EA7STUDENTCOURSES sc on sdrcl.ea7studentcoursesid = sc.ea7studentcoursesid
		LEFT OUTER JOIN (select coalesce(sum(sdrcl2.creditsused), 0) 'creditssum', SCI.EA7COURSESID from @studentcourseinfo sci 
					inner join ea7studentcourses sc2 on sc2.ea7studentcoursesid = sci.ea7studentcoursesid 
					inner join EA7STUDENTDEGREQCOURSELINKS sdrcl2 on sc2.ea7studentcoursesid = sdrcl2.ea7studentcoursesid
					inner join EA7STUDENTDEGREEREQCOURSES sdrc3 on sdrc3.EA7STUDENTDEGREEREQCOURSESID = sdrcl2.EA7STUDENTDEGREEREQCOURSESID --this is to ensure these courses were counted towards the students' requirements
					INNER JOIN EA7STUDENTDEGREEREQS sdr2 ON sdrc3.EA7STUDENTDEGREEREQSID = sdr2.EA7STUDENTDEGREEREQSID
					INNER JOIN EA7STUDENTDEGREES sd2 ON sdr2.EA7STUDENTDEGREESID = sd2.EA7STUDENTDEGREESID
					where sc2.isothercourse = 0 AND sd2.ISWHATIF = @bIsWhatIf AND (sdr2.EA7STUDENTDEGREESID IN (SELECT sd3.EA7STUDENTDEGREESID FROM EA7STUDENTDEGREES sd3 
														LEFT JOIN EA7STUDENTDEGREES sd4 ON sd3.PARENTDEGREEID = sd4.EA7STUDENTDEGREESID
														WHERE (sd3.DEGREETYPE = 1 AND sd3.EA7STUDENTDEGREESID = @PARENTSTUDDEGREEID) OR
														(sd3.DEGREETYPE IN (2, 3) AND sd3.PARENTDEGREEID = @PARENTSTUDDEGREEID) OR
														(sd3.DEGREETYPE IN (4, 5) AND sd4.PARENTDEGREEID = @PARENTSTUDDEGREEID)))
					GROUP BY SCI.EA7COURSESID) SCISUM on scisum.ea7coursesid = sc.ea7coursesid
		WHERE sc.ISOTHERCOURSE =-1 AND sc.EA7STUDENTSID= @EA7STUDENTSID AND SD.ISWHATIF = @bIsWhatIf
		AND (sdr.EA7STUDENTDEGREESID IN (SELECT sd.EA7STUDENTDEGREESID FROM EA7STUDENTDEGREES sd 
			LEFT JOIN EA7STUDENTDEGREES sd2 ON sd.PARENTDEGREEID = sd2.EA7STUDENTDEGREESID
			WHERE (sd.DEGREETYPE = 1 AND sd.EA7STUDENTDEGREESID = @PARENTSTUDDEGREEID) OR
			(sd.DEGREETYPE IN (2, 3) AND sd.PARENTDEGREEID = @PARENTSTUDDEGREEID) OR
			(sd.DEGREETYPE IN (4, 5) AND sd2.PARENTDEGREEID = @PARENTSTUDDEGREEID)))
		GROUP BY sdrc.creditsused, scisum.creditssum, sc.ea7coursesid) sdrc2

		set @MaxOtherCredits = 0

		--figure the number of other credits allowed
		select @MaxOtherCredits = (coalesce(MINIMUMCREDITS,0) - coalesce(INHOUSECREDITS,0))
		FROM EA7DEGREES
		WHERE EA7DEGREESID = @EA7DEGREESID AND RECORDTYPE = 1

		IF @MaxOtherCredits = 0
		BEGIN
			--we have a major/minor/concentration/option, need to check the 'parent'
			SELECT @MaxOtherCredits = (coalesce(ea7degrees.MINIMUMCREDITS,0) - coalesce(ea7degrees.INHOUSECREDITS,0))
			FROM EA7DEGREES MAJORS INNER JOIN EA7DEGREES ON MAJORS.FORDEGREE = EA7DEGREES.EA7DEGREESID
			WHERE majors.EA7DEGREESID = @EA7DEGREESID AND majors.MAJORMINORTYPE IN (1,2)
			
			IF @MaxOtherCredits = 0
				SELECT @MaxOtherCredits = (coalesce(ea7degrees.MINIMUMCREDITS,0) - coalesce(ea7degrees.INHOUSECREDITS,0))
				FROM EA7DEGREES OPTS 
				INNER JOIN EA7DEGREES MAJORS ON OPTS.FORMAJOR = MAJORS.EA7DEGREESID
				INNER JOIN EA7DEGREES ON MAJORS.FORDEGREE = EA7DEGREES.EA7DEGREESID
				WHERE opts.EA7DEGREESID = @EA7DEGREESID AND opts.MAJORMINORTYPE IN (3,4)

			--technically, we want an unlimited number for this scenario...
			IF @MaxOtherCredits = 0
				set @MaxOtherCredits = 10000
		END

		--CR295384-030708 SAM - if other credits used is greater than max credits, we get a negative number
		-- later on in processing.  Other credits used will only be greater if the last class used for other
		-- schools has more credits than what max other credits will allow.  
		if @OtherCreditsUsed > @MaxOtherCredits
			Set @OtherCreditsUsed = @MaxOtherCredits
				
		--CR274460-050307 ScotMe 05/04/2007- Added "top 1" for case with course being taken multiple times for credit, which the same course
		--would have two different statuses.  For the purposes of this calculation, we will only consider the first one, since we are
		--already in order by priority.  Eventually this status field should move to EA7STUDENTDEGREEREQCOURSELINKS table, but given the narrow
		--scope of this issue and the proximity to the 7.70 release, we're going to leave it for now.  99% of the time there will only ever be
		--one course here...
		SELECT top 1 @STATUS =  CASE 
					WHEN COALESCE(SUM(CREDITSAWARDED),0) > 0 THEN 2	--complete
					WHEN exists(SELECT EA7STUDENTCOURSETERMSID FROM EA7STUDENTCOURSETERMS WHERE EA7STUDENTCOURSESID = SCI.EA7STUDENTCOURSESID AND ENROLLMENTSTATUS = 2) then 1--in progress
					WHEN exists(SELECT EA7STUDENTCOURSETERMSID FROM EA7STUDENTCOURSETERMS WHERE EA7STUDENTCOURSESID = SCI.EA7STUDENTCOURSESID AND ENROLLMENTSTATUS = 1) then 4--Planned
					WHEN sci.OTHERCOURSE = -1 THEN 4  --CR310381-102308 other course is either planned or completed if it exists as a studentcourse
					ELSE 3 
					END
		FROM @STUDENTCOURSEINFO SCI
		WHERE SCI.EA7COURSESID = @EA7COURSESID
		GROUP BY SCI.EA7STUDENTCOURSESID, sci.OTHERCOURSE

		--clear this guy out- if no records are in the following SQL query, old value stays in there, which is bad!
		set @CREDITSUSED = 0		

		SELECT @CREDITSUSED = coalesce(coalesce(sum(coalesce(SDRC.CREDITSUSED,0)),0) + coalesce(SDR.WAIVEDCREDITS,0) + coalesce(SDR.SUBSTITUTEDCREDITS,0) + coalesce(SDR.EXEMPTEDCREDITS,0) ,0)
		FROM EA7STUDENTDEGREEREQS AS SDR 
		LEFT OUTER JOIN EA7STUDENTDEGREEREQCOURSES AS SDRC ON SDR.EA7STUDENTDEGREEREQSID = SDRC.EA7STUDENTDEGREEREQSID
		LEFT OUTER JOIN EA7STUDENTDEGREEREQGROUPS SDRG ON SDRG.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID AND SDRG.GROUPID = SDRC.GROUPID 
		where sdr.ea7studentdegreereqsid = @EA7STUDENTDEGREEREQSID AND ((SDRG.INCLUDED = -1) OR (SDRG.EA7STUDENTDEGREEREQGROUPSID IS NULL)) 
		GROUP BY SDR.WAIVEDCREDITS, SDR.SUBSTITUTEDCREDITS, SDR.EXEMPTEDCREDITS
		
		set @CurrentOtherCredits = 0
		set @CurrentCredits = 0
		set @CreditsToUse = 0
		set @numOtherCourses = 0 --CR315304-021809
		
		select @numOtherCourses = count(sci.ea7studentcoursesid), @CurrentOTherCredits = coalesce(sum(CREDITSAWARDED),0)
		FROM @STUDENTCOURSEINFO sci
		--CR315304-021809 - first issue - looks like othercourse is also subject to can satisfy restrictions
		WHERE sci.EA7COURSESID = @EA7COURSESID and sci.othercourse = -1 and ( @OtherCreditsUsed < @MaxOtherCredits)		
	    AND (COALESCE(@CANSATISFY,0) <> 2 OR sci.EA7STUDENTCOURSESID not in (select sdrcl.ea7studentcoursesid
										  from @TempCachedCourseLinks sdrcl
										  inner join ea7studentcourses sc on sdrcl.ea7studentcoursesid = sc.ea7studentcoursesid
										  where sc.ea7coursesid = @EA7COURSESID))
		  AND (COALESCE(@CANSATISFY,0) <> 3 OR sci.EA7STUDENTCOURSESID not in (select ea7studentcoursesid 
										   from @TempCachedCourseLinks sdrcl2 
										inner join ea7studentdegreereqcourses sdrc2 on sdrcl2.ea7studentdegreereqcoursesid = sdrc2.ea7studentdegreereqcoursesid
										inner join ea7studentdegreereqs sdr2 on sdrc2.ea7studentdegreereqsid = sdr2.ea7studentdegreereqsid
										where sdr2.ea7studentdegreesID = @EA7STUDENTDEGREESID))
		  AND (COALESCE(@CANSATISFY,0) <> 4 OR sci.EA7STUDENTCOURSESID not in(select ea7studentcoursesid 
											 from @TempCachedCourseLinks sdrcl3 
										inner join ea7studentdegreereqcourses sdrc3 on sdrcl3.ea7studentdegreereqcoursesid = sdrc3.ea7studentdegreereqcoursesid
											 inner join ea7studentdegreereqs sdr3 on sdrc3.ea7studentdegreereqsid = sdr3.ea7studentdegreereqsid
											 inner join ea7studentdegrees sd3 on sdr3.ea7studentdegreesid = sd3.ea7studentdegreesid
											 where sdr3.ea7studentdegreesID <> @EA7STUDENTDEGREESID and sd3.ea7studentsid = @EA7STUDENTSID))
		--AND COALESCE(tccl.CANSATISFY,0)	<> 2											 
		--CR310473-102408 left join lists too many multiple courses
		AND NOT EXISTS (select tccl.CANSATISFY from @TempCachedCourseLinks tccl where sci.ea7studentcoursesID = tccl.ea7studentcoursesID and tccl.CANSATISFY = 2) 


		--08.26.08 CR306801-082008 This change is to fix the scenario where a course should not be listed because it satisfies a requirement above and its 'can satisfy' field is set to 'only this requirement'			
		--These are the credits for the courses that will be added to StudentCourseLinks
		select @CurrentCredits = coalesce(sum(CREDITSAWARDED),0)
		FROM @STUDENTCOURSEINFO sci
		--LEFT OUTER JOIN @TempCachedCourseLinks tccl on sci.ea7studentcoursesID = tccl.ea7studentcoursesID --08.26.08 CR306801-082008				
		WHERE sci.EA7COURSESID = @EA7COURSESID and sci.othercourse = 0 		
		  AND (COALESCE(@CANSATISFY,0) <> 2 OR sci.EA7STUDENTCOURSESID not in (select sdrcl.ea7studentcoursesid
										  from @TempCachedCourseLinks sdrcl
										  inner join ea7studentcourses sc on sdrcl.ea7studentcoursesid = sc.ea7studentcoursesid
										  where sc.ea7coursesid = @EA7COURSESID))
		  AND (COALESCE(@CANSATISFY,0) <> 3 OR sci.EA7STUDENTCOURSESID not in (select ea7studentcoursesid 
										   from @TempCachedCourseLinks sdrcl2 
										inner join ea7studentdegreereqcourses sdrc2 on sdrcl2.ea7studentdegreereqcoursesid = sdrc2.ea7studentdegreereqcoursesid
										inner join ea7studentdegreereqs sdr2 on sdrc2.ea7studentdegreereqsid = sdr2.ea7studentdegreereqsid
										where sdr2.ea7studentdegreesID = @EA7STUDENTDEGREESID))
		  AND (COALESCE(@CANSATISFY,0) <> 4 OR sci.EA7STUDENTCOURSESID not in(select ea7studentcoursesid 
											 from @TempCachedCourseLinks sdrcl3 
										inner join ea7studentdegreereqcourses sdrc3 on sdrcl3.ea7studentdegreereqcoursesid = sdrc3.ea7studentdegreereqcoursesid
											 inner join ea7studentdegreereqs sdr3 on sdrc3.ea7studentdegreereqsid = sdr3.ea7studentdegreereqsid
											 inner join ea7studentdegrees sd3 on sdr3.ea7studentdegreesid = sd3.ea7studentdegreesid
											 where sdr3.ea7studentdegreesID <> @EA7STUDENTDEGREESID and sd3.ea7studentsid = @EA7STUDENTSID))
		--AND COALESCE(tccl.CANSATISFY,0)	<> 2											 
		--CR310473-102408 left join lists too many multiple courses
		AND NOT EXISTS (select tccl.CANSATISFY from @TempCachedCourseLinks tccl where sci.ea7studentcoursesID = tccl.ea7studentcoursesID and tccl.CANSATISFY = 2) 
		
		--CR274460-050307 SAM - for the case of courses taken multiple times for credit, we can have a course that is both
		--completed and in progress (two different Academic years or sessions, same course id).
		select @CurrentCredits = coalesce(@CurrentCredits,0) + coalesce(sum(CREDITSATTEMPTED),0)
		FROM @STUDENTCOURSEINFO
		WHERE EA7COURSESID = @EA7COURSESID and coalesce(CREDITSAWARDED,0) = 0 --and othercourse = 0  --CR310381-102308 allow other course to be listed as planned

		--02/20/2009 SH CR315304-021809 other courses credits can't exceed max other credits
		--05/14/2007 SAM CR274460-050307 - get specific student course data to operate on 
		--05/21/2007 SAM CR275707-052107 - added case statement to handle 0 versus null, if creditsawarded is zero, then we still want to use credits attempted
		--02/19/2008 QW	 CR289983-121207 - If the translation table has no numeric equivalent defined, gradevalue could be null, we need to sort by creditsawarded
		--11/23/2011 ScottKa CR340150-112211 - Removing any multiple courses that do not meet the minimum grade required.
		--09/12/2012 CR344938-083112 restrict multiple instances of the same course that don't meet the min grade later in the processing (CR340150-112211)
		insert into @MultiCourses(EA7STUDENTCOURSESID, credits)
		select sci.EA7STUDENTCOURSESID, 
		case sci.othercourse 
		when 0 then
			case
				when coalesce(creditsawarded, 0) > 0 then creditsawarded
				else coalesce(creditsattempted,0)
			end
		else 
			case @numOtherCourses
			when 0 then 0
			else
				case
					when (@OTHERCREDITSUSED + @CurrentOtherCredits) <= @MAXOTHERCREDITS then @CurrentOtherCredits/@numOtherCourses
					else (@MaxOtherCredits - @OtherCreditsUsed)/@numOtherCourses
				end
			end
		end
		from @Studentcourseinfo sci
		--LEFT OUTER JOIN @TempCachedCourseLinks tccl on sci.ea7studentcoursesID = tccl.ea7studentcoursesID --08.26.08 CR306801-082008			
		where sci.ea7coursesid = @EA7COURSESID
			--08.26.08 CR306801-082008 This change is to fix the scenario where a course should not be listed because it satisfies a requirement above and its 'can satisfy' field is set to 'only this requirement'		
		  AND (COALESCE(@CANSATISFY,0) <> 2 OR sci.EA7STUDENTCOURSESID not in (select sdrcl.ea7studentcoursesid
										  from @TempCachedCourseLinks sdrcl
										  inner join ea7studentcourses sc on sdrcl.ea7studentcoursesid = sc.ea7studentcoursesid
										  where sc.ea7coursesid = @EA7COURSESID))
		  AND (COALESCE(@CANSATISFY,0) <> 3 OR sci.EA7STUDENTCOURSESID not in (select ea7studentcoursesid 
										   from @TempCachedCourseLinks sdrcl2 
										inner join ea7studentdegreereqcourses sdrc2 on sdrcl2.ea7studentdegreereqcoursesid = sdrc2.ea7studentdegreereqcoursesid
										inner join ea7studentdegreereqs sdr2 on sdrc2.ea7studentdegreereqsid = sdr2.ea7studentdegreereqsid
										where sdr2.ea7studentdegreesID = @EA7STUDENTDEGREESID))
		  AND (COALESCE(@CANSATISFY,0) <> 4 OR sci.EA7STUDENTCOURSESID not in(select ea7studentcoursesid 
											 from @TempCachedCourseLinks sdrcl3 
										inner join ea7studentdegreereqcourses sdrc3 on sdrcl3.ea7studentdegreereqcoursesid = sdrc3.ea7studentdegreereqcoursesid
											 inner join ea7studentdegreereqs sdr3 on sdrc3.ea7studentdegreereqsid = sdr3.ea7studentdegreereqsid
											 inner join ea7studentdegrees sd3 on sdr3.ea7studentdegreesid = sd3.ea7studentdegreesid
											 where sdr3.ea7studentdegreesID <> @EA7STUDENTDEGREESID and sd3.ea7studentsid = @EA7STUDENTSID))		
		--AND COALESCE(tccl.CANSATISFY,0)	<> 2
		--CR310473-102408 left join lists too many multiple courses
		AND NOT EXISTS (select tccl.CANSATISFY from @TempCachedCourseLinks tccl where sci.ea7studentcoursesID = tccl.ea7studentcoursesID and tccl.CANSATISFY = 2) 
		order by gradevalue desc, creditsawarded desc
		
		--05/14/2007 SAM CR274460-050307 calculate running total to see how many credits we can use of the current course (even if taken
		--multiple times)
		UPDATE @MultiCourses
		set RunningTotal = (select coalesce(sum(credits),0) from @MultiCourses MC where MC.MultiID < MC2.MultiID)
		from @MultiCourses MC2

		IF exists(SELECT sci.EA7COURSESID 
			  FROM @STUDENTCOURSEINFO sci
			  WHERE sci.EA7COURSESID = @EA7COURSESID AND ((sci.OTHERCOURSE = -1 AND @OtherCreditsUsed < @MaxOtherCredits) OR sci.OTHERCOURSE=0)
			  AND (COALESCE(@CANSATISFY,0) <> 2 OR sci.EA7STUDENTCOURSESID not in (select sdrcl.ea7studentcoursesid
											  from @TempCachedCourseLinks sdrcl
											  inner join ea7studentcourses sc on sdrcl.ea7studentcoursesid = sc.ea7studentcoursesid
											  where sc.ea7coursesid = @EA7COURSESID))
			  AND (COALESCE(@CANSATISFY,0) <> 3 OR sci.EA7STUDENTCOURSESID not in (select ea7studentcoursesid 
											   from @TempCachedCourseLinks sdrcl2 
											inner join ea7studentdegreereqcourses sdrc2 on sdrcl2.ea7studentdegreereqcoursesid = sdrc2.ea7studentdegreereqcoursesid
											inner join ea7studentdegreereqs sdr2 on sdrc2.ea7studentdegreereqsid = sdr2.ea7studentdegreereqsid
											where sdr2.ea7studentdegreesID = @EA7STUDENTDEGREESID))
			  AND (COALESCE(@CANSATISFY,0) <> 4 OR sci.EA7STUDENTCOURSESID not in(select ea7studentcoursesid 
												 from @TempCachedCourseLinks sdrcl3 
											inner join ea7studentdegreereqcourses sdrc3 on sdrcl3.ea7studentdegreereqcoursesid = sdrc3.ea7studentdegreereqcoursesid
												 inner join ea7studentdegreereqs sdr3 on sdrc3.ea7studentdegreereqsid = sdr3.ea7studentdegreereqsid
												 inner join ea7studentdegrees sd3 on sdr3.ea7studentdegreesid = sd3.ea7studentdegreesid
												 where sdr3.ea7studentdegreesID <> @EA7STUDENTDEGREESID and sd3.ea7studentsid = @EA7STUDENTSID))
			 --CR323949-082509,CR323614-081309,CR323468-081009 Make sure the requirement has not already been used in a completed degree if Can Satisfy is "only this req" or "only this degree"
			 AND (Coalesce(@CANSATISFY,0) not in (2,4) or sci.EA7STUDENTCOURSESID not in(select SDRC.EA7COURSESID from EA7STUDENTDEGREEREQCOURSES SDRC
											inner join EA7STUDENTDEGREEREQS SDR on SDR.EA7STUDENTDEGREEREQSID =SDRC.EA7STUDENTDEGREEREQSID
											inner join EA7STUDENTDEGREES SD on SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID
											where SD.EA7STUDENTSID = @EA7STUDENTSID and SD.STATUS = 2 ))												 
												 )
		BEGIN
			BEGIN
				--AlisonBu - 7.82 Dev - Be sure Minimum Grade Requirement is met before processing
				DECLARE @CourseGradeValue numeric (19,7)
				DECLARE @CourseGrade_TTID	int
				DECLARE @CourseGrade_TTSeq int
				DECLARE @Req_MinGradeNumEquivalent numeric (19,7)
				DECLARE @Req_TTSeq int
				DECLARE @MeetsMinGradeReq smallint
				DECLARE @GradeType int
				Set @MeetsMinGradeReq = -1
				
				If @MinimumGradeRequired = -1
					BEGIN	
					SELECT top 1 @CourseGradeValue = SCI.GRADEVALUE, --CR341302-020312 Removed IsNull to allow null check later
						@CourseGrade_TTID = SCI.EA7TRANSLATIONSID,
						@CourseGrade_TTSeq = SCI.TTSEQUENCE,
						@GradeType = SCI.GRADETYPE
					FROM @STUDENTCOURSEINFO SCI
					WHERE SCI.EA7COURSESID = @EA7COURSESID
										
					If @MinimumGradeTE Is Null or LEN(@MinimumGradeTE) = 0
						--Minimum Grade Numeric Value was selected on the Requirement; if there is no CourseGradeValue (@CourseGradeValue is null), the course grade must use a Translation Table 
						--entry that does not have a Numeric Equivalent.  If the degree Req uses a numeric value as the minimum grade, 
						--we have no way to match them so it will not pass this check.
						BEGIN
						If @CourseGradeValue is null or IsNull(@CourseGradeValue,0) < @MinimumGradeValue
							Set @MeetsMinGradeReq = 0
						END
					Else
						
						--Minimum Grade TableEntry was selected on the Requirement
						BEGIN
							--CR341362-020812 reset variables
							set @Req_MinGradeNumEquivalent = null
							set @Req_TTSeq = null
							
							--CR330211-061710 When student grade is a letter grade, use the numeric equivalent or sequence if numeric equivalent is not defined, 
							--Otherwise the student grade is a numeric grade, so use the cutoff value to compare; Grade type = 1 is Grade only, 2 = Numeric
							Select @Req_MinGradeNumEquivalent = case when @GradeType = 1 then NUMEQUIVALENT else CUTOFFGRADE end, @Req_TTSeq = SEQUENCE 
							From EA7TRANSLATIONENTRIES TE 
							WHERE TE.EA7TRANSLATIONSID = @CourseGrade_TTID and 
							TE.GRADE = @MinimumGradeTE
							
							If @Req_MinGradeNumEquivalent Is Null
								BEGIN
									--if there is no numeric equivalent, use the sequence
									If ISNULL(@Req_TTSeq,0) <> 0
										BEGIN
										If @Req_TTSeq < @CourseGrade_TTSeq
											Set @MeetsMinGradeReq = 0
										END
									else
										--There is no sequence so the Table Entry must not exist in the same Translation Table
										--We have no way to match these so it will not pass this check.
										Set @MeetsMinGradeReq = 0
								End
							else
								-- CR341302-020312 grade value should be null if course grade is blank, not 0
								-- CR328888-032510, If the course grade is blank, @MeetsMinGradeReq needs to be 1
								-- to allow classes to be listed as in-progress
								If (@CourseGradeValue is not null or @CourseGrade_TTSeq is not null) and ISNULL(@CourseGradeValue, 0) < @Req_MinGradeNumEquivalent
									Set @MeetsMinGradeReq = 0
						END
					END
			END
			
			If @MeetsMinGradeReq = -1
			
			BEGIN
			
				--CR308138-091108 Other course was awarded credit, but a regular course was also awarded credit, so added @CurrentCredits to creditstouse even if there currentothercredits > 0		
				IF @CurrentOtherCredits > 0	--awarded credit in other course
					IF (@OTHERCREDITSUSED + @CurrentOtherCredits) <= @MAXOTHERCREDITS
						set @CreditsToUse = @CurrentOtherCredits + @CurrentCredits
					ELSE
						set @CreditsToUse = (@MaxOtherCredits - @OtherCreditsUsed) + @CurrentCredits --only use part of the credits as we have more than we need
				ELSE	--awarded credit in regular course
					set @CreditsToUse = @CurrentCredits

				IF (@CREDITSUSED + @CreditsToUse) > @CREDITSREQUIREDTO
					IF @GroupID IS NULL OR ((SELECT SDRG.STATUS FROM EA7STUDENTDEGREEREQGROUPS SDRG WHERE SDRG.GROUPID = @GroupID AND SDRG.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID) = 2)
					--CR307580-090408 negative values were being set for the creditused field here. 
						if @CreditsUsed > @CreditsRequiredTo
							SET @CreditsToUse = @CreditsRequiredTo
						else				
							SET @CreditsToUse = @CreditsRequiredTo - @CreditsUsed
					ELSE
					BEGIN
						SET @GroupCreditsLeft = (SELECT COALESCE(SDRG.CREDITSREQUIRED,0) - (COALESCE(SDRG.CREDITSAPPLIED,0) + COALESCE(SDRG.WAIVEDCREDITS,0) + COALESCE(SDRG.SUBSTITUTEDCREDITS,0) + COALESCE(SDRG.EXEMPTEDCREDITS,0)) FROM EA7STUDENTDEGREEREQGROUPS SDRG WHERE SDRG.GROUPID = @GroupID AND SDRG.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID)
						--CR307580-090408 negative values were being set for the creditused field here. 
						IF @GroupCreditsLeft < @CreditsToUse AND @GroupCreditsLeft >= 0
							SET @CreditsToUse = @GroupCreditsLeft
					END

				INSERT INTO EA7STUDENTDEGREEREQCOURSES(EA7STUDENTDEGREEREQSID, EA7DEGREEREQCOURSESID, EA7COURSESID, STATUS, CREDITSUSED, GROUPID)
				VALUES(@EA7STUDENTDEGREEREQSID,@EA7DEGREEREQCOURSESID,@EA7COURSESID, @STATUS, @CREDITSTOUSE, @GroupID)

				SET @REQCOURSESID = @@IDENTITY

				--MaryRu 10.15.2009 CR325520-101409  - Should only be checking that a grade exists.
				--When filling @STUDENTCOURSEINFO, we were coalescing the TranslationEntry.NumericEquivalent (of a letter grade) with the EA7StudentGrades.GradeValue (for numeric grades), 
				--   into the one field GradeValue, but that will not always let us know if a grade exists. If the TranslationEntry has no numeric equivalent, GradeValue would still be null
				--   even though a grade exists. And if a numeric grade was entered, checking that it is greater than 0 excludes the valid grade of 0. Also, EA7TranslationsID is the ID of 
				--   the Translation Table, NOT the ID of a grade in the Translation Table, so checking it here only insures that graded marking columns will always be seen as having a grade, 
				--   whether they have one or not.
				--So from the following two Inserts into EA7STUDENTDEGREQCOURSELINKS and @TempCachedCourseLinks,
				--   ((LEN(SCI.GRADEVALUE) > 0) OR (ISNULL(SCI.EA7TRANSLATIONSID,0) > 0))
				--     Replaced the GradeValue check with (SCI.EA7SG_GRADEVALUE is not null OR SCI.EA7SG_EA7TRANSLATIONENTRIESID is not null)
				--     Removed the SCI.EA7TRANSLATIONSID check
				--So the final check for complete becomes   (If credits are awarded  OR  (if a grade exists and if the requirement is satisfied by tasks complete))

				--05/07/2007 SAM CR274460-050307 & CR274547-050407 - added status field to ea7studentdegreqcourselinks
				--05/14/2007 SAM CR274460-050307 - use our calculated "running total" to keep track of how many credits are available
				--09/12/2012 CR344938-083112 restrict multiple instances of the same course that don't meet the min grade later in the processing (CR340150-112211)
				INSERT INTO EA7STUDENTDEGREQCOURSELINKS(EA7STUDENTDEGREEREQCOURSESID, EA7STUDENTCOURSESID, STATUS, CREDITSUSED)
				SELECT @REQCOURSESID, sci.EA7STUDENTCOURSESID, 
				CASE
					WHEN (COALESCE(SUM(CREDITSAWARDED),0) > 0) OR ( (SCI.EA7SG_GRADEVALUE is not null OR SCI.EA7SG_EA7TRANSLATIONENTRIESID is not null) AND (DR.COMPLETEDUSING = 2) ) THEN 2	--complete
					WHEN exists(SELECT EA7STUDENTCOURSETERMSID FROM EA7STUDENTCOURSETERMS WHERE EA7STUDENTCOURSESID = SCI.EA7STUDENTCOURSESID AND ENROLLMENTSTATUS = 2) then 1--in progress
					WHEN exists(SELECT EA7STUDENTCOURSETERMSID FROM EA7STUDENTCOURSETERMS WHERE EA7STUDENTCOURSESID = SCI.EA7STUDENTCOURSESID AND ENROLLMENTSTATUS = 1) then 4--Planned
					WHEN sci.OTHERCOURSE = -1 THEN 4 --CR310381-102308 allow othercourse to be planned
					ELSE 3 
				END,
				CASE
					when (mc.RunningTotal + mc.credits) <= @CreditsToUse then mc.credits
					else @CreditsToUse - mc.runningTotal
				END
				FROM @STUDENTCOURSEINFO SCI
				inner join EA7STUDENTDEGREEREQCOURSES SDRC on  SDRC.EA7STUDENTDEGREEREQCOURSESID = @REQCOURSESID
				inner join EA7STUDENTDEGREEREQS SDR on SDRC.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
				inner join EA7DEGREEREQUIREMENTS DR on SDR.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID
				inner join @MultiCourses MC ON sci.ea7studentcoursesID = mc.ea7studentcoursesid
				--LEFT OUTER JOIN @TempCachedCourseLinks tccl on sci.ea7studentcoursesID = tccl.ea7studentcoursesID --08.26.08 CR306801-082008
				LEFT OUTER JOIN EA7TRANSLATIONENTRIES TE on sci.EA7SG_EA7TRANSLATIONENTRIESID = TE.EA7TRANSLATIONENTRIESID
				LEFT OUTER JOIN EA7TRANSLATIONENTRIES TE2 on TE.EA7TRANSLATIONSID = TE2.EA7TRANSLATIONSID and TE2.GRADE = @MinimumGradeTE	
				WHERE SCI.EA7COURSESID = @EA7COURSESID and ((OTHERCOURSE = -1 AND @OtherCreditsUsed < @MaxOtherCredits) OR OTHERCOURSE=0)
				AND (mc.runningtotal < @CreditsToUse) OR (DR.COMPLETEDUSING = 2)
				--08.26.08 CR306801-082008 This change is to fix the scenario where a course should not be listed because it satisfies a requirement above and its 'can satisfy' field is set to 'only this requirement'			
				AND (COALESCE(@CANSATISFY,0) <> 2 OR sci.EA7STUDENTCOURSESID not in (select sdrcl.ea7studentcoursesid
												  from @TempCachedCourseLinks sdrcl
												  inner join ea7studentcourses sc on sdrcl.ea7studentcoursesid = sc.ea7studentcoursesid
												  where sc.ea7coursesid = @EA7COURSESID))
				AND (COALESCE(@CANSATISFY,0) <> 3 OR sci.EA7STUDENTCOURSESID not in (select ea7studentcoursesid 
												   from @TempCachedCourseLinks sdrcl2 
													inner join ea7studentdegreereqcourses sdrc2 on sdrcl2.ea7studentdegreereqcoursesid = sdrc2.ea7studentdegreereqcoursesid
													inner join ea7studentdegreereqs sdr2 on sdrc2.ea7studentdegreereqsid = sdr2.ea7studentdegreereqsid
													where sdr2.ea7studentdegreesID = @EA7STUDENTDEGREESID))
				AND (COALESCE(@CANSATISFY,0) <> 4 OR sci.EA7STUDENTCOURSESID not in(select ea7studentcoursesid 
													 from @TempCachedCourseLinks sdrcl3 
													inner join ea7studentdegreereqcourses sdrc3 on sdrcl3.ea7studentdegreereqcoursesid = sdrc3.ea7studentdegreereqcoursesid
													 inner join ea7studentdegreereqs sdr3 on sdrc3.ea7studentdegreereqsid = sdr3.ea7studentdegreereqsid
													 inner join ea7studentdegrees sd3 on sdr3.ea7studentdegreesid = sd3.ea7studentdegreesid
													 where sdr3.ea7studentdegreesID <> @EA7STUDENTDEGREESID and sd3.ea7studentsid = @EA7STUDENTSID))
				--AND COALESCE(tccl.CANSATISFY,0)	<> 2									
				--CR310473-102408 left join lists too many multiple courses
				AND NOT EXISTS (select tccl.CANSATISFY from @TempCachedCourseLinks tccl where sci.ea7studentcoursesID = tccl.ea7studentcoursesID and tccl.CANSATISFY = 2) 			
				AND ((ISNULL(@MinimumGradeRequired,0) = 0) OR ((sci.TTSEQUENCE <= TE2.SEQUENCE) OR (SCI.GRADEVALUE >= @minimumGradeValue)) OR ((sci.TTSEQUENCE IS NULL) and (SCI.GRADEVALUE IS NULL)))
				GROUP BY sci.EA7STUDENTCOURSESID, mc.RunningTotal, mc.credits, sci.OTHERCOURSE, sci.GRADEVALUE, DR.COMPLETEDUSING, SCI.EA7SG_GRADEVALUE, SCI.EA7SG_EA7TRANSLATIONENTRIESID

				--08.26.08 CR306801-082008 This change is to fix the scenario where a course should not be listed because it satisfies a requirement above and its 'can satisfy' field is set to 'only this requirement'			
				--Insert the same thing into the cache table so we can query against it instead of what's in the database
				--09/12/2012 CR344938-083112 restrict multiple instances of the same course that don't meet the min grade later in the processing (CR340150-112211)
				INSERT INTO @TempCachedCourseLinks(EA7STUDENTDEGREEREQCOURSESID, EA7STUDENTCOURSESID, STATUS, CREDITSUSED, CANSATISFY)
				SELECT @REQCOURSESID, sci.EA7STUDENTCOURSESID, 
				CASE 
					WHEN (COALESCE(SUM(CREDITSAWARDED),0) > 0) OR ( (SCI.EA7SG_GRADEVALUE is not null OR SCI.EA7SG_EA7TRANSLATIONENTRIESID is not null) AND (DR.COMPLETEDUSING = 2) ) THEN 2	--complete
					WHEN exists(SELECT EA7STUDENTCOURSETERMSID FROM EA7STUDENTCOURSETERMS WHERE EA7STUDENTCOURSESID = SCI.EA7STUDENTCOURSESID AND ENROLLMENTSTATUS = 2) then 1--in progress
					WHEN exists(SELECT EA7STUDENTCOURSETERMSID FROM EA7STUDENTCOURSETERMS WHERE EA7STUDENTCOURSESID = SCI.EA7STUDENTCOURSESID AND ENROLLMENTSTATUS = 1) then 4--Planned
					WHEN sci.OTHERCOURSE = -1 THEN 4 --CR310381-102308 allow othercourse to be planned
					ELSE 3 
				END,
				CASE
					when (mc.RunningTotal + mc.credits) <= @CreditsToUse then mc.credits
					else @CreditsToUse - mc.runningTotal
				END,
				@CANSATISFY
				FROM @STUDENTCOURSEINFO SCI
				inner join EA7STUDENTDEGREEREQCOURSES SDRC on  SDRC.EA7STUDENTDEGREEREQCOURSESID = @REQCOURSESID
				inner join EA7STUDENTDEGREEREQS SDR on SDRC.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
				inner join EA7DEGREEREQUIREMENTS DR on SDR.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID
				inner join @MultiCourses MC ON sci.ea7studentcoursesID = mc.ea7studentcoursesid
				--LEFT OUTER JOIN @TempCachedCourseLinks tccl on sci.ea7studentcoursesID = tccl.ea7studentcoursesID --08.26.08 CR306801-082008			
				LEFT OUTER JOIN EA7TRANSLATIONENTRIES TE on sci.EA7SG_EA7TRANSLATIONENTRIESID = TE.EA7TRANSLATIONENTRIESID
				LEFT OUTER JOIN EA7TRANSLATIONENTRIES TE2 on TE.EA7TRANSLATIONSID = TE2.EA7TRANSLATIONSID and TE2.GRADE = @MinimumGradeTE	
				WHERE SCI.EA7COURSESID = @EA7COURSESID and ((OTHERCOURSE = -1 AND @OtherCreditsUsed < @MaxOtherCredits) OR OTHERCOURSE=0)
				AND (mc.runningtotal < @CreditsToUse) OR (DR.COMPLETEDUSING = 2)
				AND (COALESCE(@CANSATISFY,0) <> 2 OR sci.EA7STUDENTCOURSESID not in (select sdrcl.ea7studentcoursesid
												  from @TempCachedCourseLinks sdrcl
												  inner join ea7studentcourses sc on sdrcl.ea7studentcoursesid = sc.ea7studentcoursesid
												  where sc.ea7coursesid = @EA7COURSESID))
				AND (COALESCE(@CANSATISFY,0) <> 3 OR sci.EA7STUDENTCOURSESID not in (select ea7studentcoursesid 
												   from @TempCachedCourseLinks sdrcl2 
												inner join ea7studentdegreereqcourses sdrc2 on sdrcl2.ea7studentdegreereqcoursesid = sdrc2.ea7studentdegreereqcoursesid
												inner join ea7studentdegreereqs sdr2 on sdrc2.ea7studentdegreereqsid = sdr2.ea7studentdegreereqsid
												where sdr2.ea7studentdegreesID = @EA7STUDENTDEGREESID))
				AND (COALESCE(@CANSATISFY,0) <> 4 OR sci.EA7STUDENTCOURSESID not in(select ea7studentcoursesid 
												from @TempCachedCourseLinks sdrcl3 
												inner join ea7studentdegreereqcourses sdrc3 on sdrcl3.ea7studentdegreereqcoursesid = sdrc3.ea7studentdegreereqcoursesid
												inner join ea7studentdegreereqs sdr3 on sdrc3.ea7studentdegreereqsid = sdr3.ea7studentdegreereqsid
												inner join ea7studentdegrees sd3 on sdr3.ea7studentdegreesid = sd3.ea7studentdegreesid
												where sdr3.ea7studentdegreesID <> @EA7STUDENTDEGREESID and sd3.ea7studentsid = @EA7STUDENTSID))
				--AND COALESCE(tccl.CANSATISFY,0)	<> 2	
				--CR310473-102408 left join lists too many multiple courses
				AND NOT EXISTS (select tccl.CANSATISFY from @TempCachedCourseLinks tccl where sci.ea7studentcoursesID = tccl.ea7studentcoursesID and tccl.CANSATISFY = 2) 
				AND ((ISNULL(@MinimumGradeRequired,0) = 0) OR ((sci.TTSEQUENCE <= TE2.SEQUENCE) OR (SCI.GRADEVALUE >= @minimumGradeValue)) OR ((sci.TTSEQUENCE IS NULL) and (SCI.GRADEVALUE IS NULL)))
				GROUP BY sci.EA7STUDENTCOURSESID, mc.RunningTotal, mc.credits, sci.OTHERCOURSE, sci.GRADEVALUE, DR.COMPLETEDUSING, SCI.EA7SG_GRADEVALUE, SCI.EA7SG_EA7TRANSLATIONENTRIESID
				
				--AlisonBu - moving this count to a variable because it was causing a bizarre error in SQL2005
				Declare @CountIDs int
				
				SELECT @CountIDs = COUNT(COALESCE(SDRCL.EA7STUDENTCOURSESID, 0)) FROM @STUDENTCOURSEINFO SCI
					LEFT JOIN EA7STUDENTDEGREQCOURSELINKS SDRCL ON SCI.EA7STUDENTCOURSESID = SDRCL.EA7STUDENTCOURSESID
					LEFT JOIN EA7STUDENTDEGREEREQCOURSES SDRC ON SDRCL.EA7STUDENTDEGREEREQCOURSESID = SDRC.EA7STUDENTDEGREEREQCOURSESID
					LEFT JOIN EA7STUDENTDEGREEREQS SDR ON SDRC.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
					LEFT JOIN EA7STUDENTDEGREES SD ON SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID AND (SD.ISWHATIF = @bIsWhatIf)
					WHERE (SCI.EA7COURSESID = @EA7COURSESID) AND (SDRCL.EA7STUDENTCOURSESID IS NULL)
				
				--set courses that cannot be used with this course due to the can satisfy field as processed
				UPDATE @TempStudentDegTable
				SET PROCESSED = -1
				FROM @TempStudentDegTable TSDT
				INNER JOIN @STUDENTDEGREEINFO SDI ON TSDT.STUDENTDEGREEINFOID = SDI.STUDENTDEGREEINFOID
				WHERE ((COALESCE(@CANSATISFY,0) = 2 AND SDI.EA7COURSESID = @EA7COURSESID)
				OR (COALESCE(@CANSATISFY,0) = 3 AND SDI.EA7COURSESID = @EA7COURSESID AND SDI.EA7STUDENTDEGREESID = @EA7STUDENTDEGREESID)
				OR (COALESCE(@CANSATISFY,0) = 4 AND SDI.EA7COURSESID = @EA7COURSESID AND SDI.EA7STUDENTDEGREESID <> @EA7STUDENTDEGREESID)		)
				--08.26.08 CR306801-082008 This change is to fix the scenario where a course should not be listed because it satisfies a requirement above and its 'can satisfy' field is set to 'only this requirement'						
				AND @CountIDs = 0
				

				--need to keep track of what courses the can satisfy status of this course invalidated in case we end up not needing this course due to group functionality, that way we can process the courses that will no longer be invalid
				INSERT INTO @CanSatisfyLinks(STUDENTDEGREEINFOID, EA7STUDENTDEGREEREQCOURSESID)
				SELECT STUDENTDEGREEINFOID, @REQCOURSESID
				FROM @STUDENTDEGREEINFO SDI 
				WHERE ((COALESCE(@CANSATISFY,0) = 2 AND SDI.EA7COURSESID = @EA7COURSESID)
				OR (COALESCE(@CANSATISFY,0) = 3 AND SDI.EA7COURSESID = @EA7COURSESID AND SDI.EA7STUDENTDEGREESID = @EA7STUDENTDEGREESID)
				OR (COALESCE(@CANSATISFY,0) = 4 AND SDI.EA7COURSESID = @EA7COURSESID AND SDI.EA7STUDENTDEGREESID <> @EA7STUDENTDEGREESID)		)
				--08.26.08 CR306801-082008 This change is to fix the scenario where a course should not be listed because it satisfies a requirement above and its 'can satisfy' field is set to 'only this requirement'						
				AND @CountIDs = 0
				
			END
		END

		--05/14/2007 SAM CR274460-050307 - clear this out...
		delete
		from @MultiCourses

		--SAM CR262099-122006 - remove the one we just processed
		--JAK - changing to just setting flag since we might need this info again due to group processing 
		UPDATE @TempStudentDegTable
		SET PROCESSED = -1
		FROM @TempStudentDegTable	
		WHERE STUDENTDEGREEINFOID = @STUDENTDEGREEINFOID

		if @GroupID IS NULL --this requirement does not use group functionality
		begin
			--ScottKa 02/05/2009 CR314555-013009
			--ScottKa 05/29/2009 7.82 Zero Credit Classes: "Excluding Completed Using": 2-Completed tasks
			UPDATE EA7STUDENTDEGREEREQS
			SET STATUS = 2
			FROM EA7STUDENTDEGREEREQS 
			WHERE EA7STUDENTDEGREEREQSID IN
				(SELECT SDR.EA7STUDENTDEGREEREQSID
				 FROM EA7STUDENTDEGREEREQS SDR 
				 INNER JOIN EA7DEGREEREQUIREMENTS DR ON SDR.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID
				 LEFT OUTER JOIN EA7STUDENTDEGREEREQCOURSES SDRC ON SDR.EA7STUDENTDEGREEREQSID = SDRC.EA7STUDENTDEGREEREQSID
				 LEFT OUTER JOIN EA7STUDENTDEGREQCOURSELINKS SDRCL ON SDRC.EA7STUDENTDEGREEREQCOURSESID = SDRCL.EA7STUDENTDEGREEREQCOURSESID
				 WHERE SDR.ea7studentdegreereqsid = @EA7STUDENTDEGREEREQSID AND SDRCL.STATUS = 2 AND DR.COMPLETEDUSING = 1
				 GROUP BY SDR.EA7STUDENTDEGREEREQSID, SDR.CREDITSREQUIRED, SDR.WAIVEDCREDITS, SDR.SUBSTITUTEDCREDITS, SDR.EXEMPTEDCREDITS
				 HAVING (coalesce(SUM(SDRCL.CREDITSUSED),0) + COALESCE(SDR.WAIVEDCREDITS,0) + COALESCE(SDR.SUBSTITUTEDCREDITS,0) + COALESCE(SDR.EXEMPTEDCREDITS,0)) >= COALESCE(SDR.CREDITSREQUIRED,0))
			
			--ScottKa 06/01/2009 7.82 Zero Credit Courses
			SELECT @NumRequiredTasks = COUNT(DRC.EA7DEGREEREQCOURSESID)
			FROM EA7STUDENTDEGREEREQS SDR
			INNER JOIN EA7DEGREEREQUIREMENTS DR ON SDR.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID
			INNER JOIN EA7DEGREEREQCOURSES DRC ON DR.EA7DEGREEREQUIREMENTSID = DRC.EA7DEGREEREQUIREMENTSID
			WHERE SDR.ea7studentdegreereqsid = @EA7STUDENTDEGREEREQSID AND DR.COMPLETEDUSING = 2
			
			--ScottKa 06/01/2009 7.82 Zero Credit Courses
			SELECT @NumCompletedTasks = COUNT(SDR.EA7STUDENTDEGREEREQSID)
			FROM EA7STUDENTDEGREEREQS SDR
			INNER JOIN EA7DEGREEREQUIREMENTS DR ON SDR.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID
			LEFT OUTER JOIN EA7STUDENTDEGREEREQCOURSES SDRC ON SDR.EA7STUDENTDEGREEREQSID = SDRC.EA7STUDENTDEGREEREQSID
			LEFT OUTER JOIN EA7STUDENTDEGREQCOURSELINKS SDRCL ON SDRC.EA7STUDENTDEGREEREQCOURSESID = SDRCL.EA7STUDENTDEGREEREQCOURSESID
			WHERE SDR.ea7studentdegreereqsid = @EA7STUDENTDEGREEREQSID AND DR.COMPLETEDUSING = 2 AND SDRCL.STATUS = 2 

			--ScottKa 06/01/2009 7.82 Zero Credit Courses
			if (@NumCompletedTasks >= @NumRequiredTasks) AND (@NumRequiredTasks <> 0)
				UPDATE EA7STUDENTDEGREEREQS
				SET STATUS = 2
				FROM EA7STUDENTDEGREEREQS 
				WHERE EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID
			
			--JAK CR307341-082908 8/29/08 - if the required number of credits is met, we do not need to process other courses
			--ScottKa 05/29/2009 7.82 Zero Credit Courses: "Excluding Completed Using": 2-Completed tasks
			if ((Select DR.COMPLETEDUSING FROM EA7STUDENTDEGREEREQS SDR INNER JOIN EA7DEGREEREQUIREMENTS DR ON DR.EA7DEGREEREQUIREMENTSID = SDR.EA7DEGREEREQUIREMENTSID WHERE EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID) = 1)
				if ((Select STATUS FROM EA7STUDENTDEGREEREQS WHERE EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID) = 2) OR ((@CREDITSUSED + @CreditsToUse) >= @CREDITSREQUIRED)
					--JAK - if the requirement is complete we do not need to process any other courses in the requirement
					UPDATE @TempStudentDegTable
					SET PROCESSED = -1
					FROM @TempStudentDegTable TSDT
					INNER JOIN @STUDENTDEGREEINFO SDI ON TSDT.STUDENTDEGREEINFOID = SDI.STUDENTDEGREEINFOID
					WHERE SDI.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID
		end
		else
		begin
			--update the number of credits that have been applied towards the group of the current course
			if @Status <> 3
				UPDATE EA7STUDENTDEGREEREQGROUPS
				SET CREDITSAPPLIED = coalesce(CREDITSAPPLIED,0) + @CreditsToUse
				FROM EA7STUDENTDEGREEREQGROUPS SDRG
				WHERE SDRG.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND SDRG.GROUPID = @GroupID
				
			--update the status of the group of the current course
			--ScottKa 02/05/2009 CR314555-013009
			UPDATE EA7STUDENTDEGREEREQGROUPS
			SET STATUS = 2
			FROM EA7STUDENTDEGREEREQGROUPS 
			WHERE EA7STUDENTDEGREEREQGROUPSID IN
				(SELECT SDRG.EA7STUDENTDEGREEREQGROUPSID
				 FROM EA7STUDENTDEGREEREQGROUPS SDRG
				 INNER JOIN EA7STUDENTDEGREEREQCOURSES SDRC ON SDRC.EA7STUDENTDEGREEREQSID = SDRG.EA7STUDENTDEGREEREQSID AND SDRC.GROUPID = SDRG.GROUPID
				 INNER JOIN EA7STUDENTDEGREQCOURSELINKS SDRCL ON SDRC.EA7STUDENTDEGREEREQCOURSESID = SDRCL.EA7STUDENTDEGREEREQCOURSESID
				 WHERE SDRG.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND SDRG.GROUPID = @GroupID AND SDRCL.STATUS = 2
				 GROUP BY SDRG.EA7STUDENTDEGREEREQGROUPSID, SDRG.CREDITSREQUIRED, SDRG.WAIVEDCREDITS, SDRG.EXEMPTEDCREDITS, SDRG.SUBSTITUTEDCREDITS
				 HAVING (coalesce(sum(SDRCL.CREDITSUSED),0) + coalesce(SDRG.WAIVEDCREDITS,0) + coalesce(SDRG.EXEMPTEDCREDITS,0) + coalesce(SDRG.SUBSTITUTEDCREDITS,0) >= coalesce(SDRG.CREDITSREQUIRED,0)))

			--CR308394-091708 if we are not using this course to satisfy this requirement, remove it from the temp cache table that keeps track of different can-satisfy courses. 
			IF @@ROWCOUNT = 0 
			BEGIN
				DELETE @TempCachedCourseLinks FROM @TempCachedCourseLinks TCCL
				WHERE TCCL.EA7STUDENTDEGREEREQCOURSESID = @REQCOURSESID
			END
			
			if (SELECT INCLUDED FROM EA7STUDENTDEGREEREQGROUPS SDRG WHERE SDRG.GROUPID = @GroupID AND SDRG.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID) = -1
				--CR343238-051812 Only use filter when using specific groups per spec 
				--set included statuses of groups that cannot be used with the current group to 0
				UPDATE EA7STUDENTDEGREEREQGROUPS
				SET INCLUDED = 0
				FROM EA7STUDENTDEGREEREQGROUPS
				WHERE EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND GROUPID IN
					(SELECT DISTINCT FILTERVALUES7.FILTERIDVALUE1
					 FROM EA7STUDENTDEGREEREQGROUPS SDRG
					 INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDRG.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
					 INNER JOIN EA7DEGREEREQGROUPS DRG ON SDR.EA7DEGREEREQUIREMENTSID = DRG.EA7DEGREEREQUIREMENTSID AND DRG.GROUPID = SDRG.GROUPID
					 INNER JOIN FILTERVALUES7 ON DRG.EA7DEGREEREQGROUPSID = FILTERVALUES7.PARENTID
					 INNER JOIN EA7DEGREEREQUIREMENTS DR ON SDR.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID
					 WHERE FILTERVALUES7.INCLUDEOPTION = 2 AND FILTERVALUES7.RECORDTYPE = 1461 AND SDRG.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND SDRG.GROUPID = @GroupID and DR.MINIMUMCREDITSFORGROUPTYPE = 2)
			else
			begin
				--the number of credits currently being satisfied by included groups
				SET @CurrentGroupTotal = (SELECT coalesce(sum(SDRG.CREDITSAPPLIED),0) + coalesce(sum(SDRG.WAIVEDCREDITS),0) + coalesce(sum(SDRG.SUBSTITUTEDCREDITS),0) + coalesce(sum(SDRG.EXEMPTEDCREDITS),0)
							 FROM EA7STUDENTDEGREEREQS SDR
							 INNER JOIN EA7STUDENTDEGREEREQGROUPS SDRG ON SDR.EA7STUDENTDEGREEREQSID = SDRG.EA7STUDENTDEGREEREQSID
							 WHERE SDR.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND SDRG.INCLUDED = -1)

				--the number of credits of the current group
				SET @NewGroupTotal = (SELECT coalesce(SDRG.CREDITSAPPLIED,0) + coalesce(SDRG.WAIVEDCREDITS,0) + coalesce(SDRG.SUBSTITUTEDCREDITS,0) + coalesce(SDRG.EXEMPTEDCREDITS,0)
								  FROM EA7STUDENTDEGREEREQGROUPS SDRG
							  WHERE SDRG.GROUPID = @GROUPID AND SDRG.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID)

				--if the current group total is higher than the old group total, update the included statuses of groups to indicate which ones are used to satisfy the req
				if @NewGroupTotal > @CurrentGroupTotal
				begin
					UPDATE EA7STUDENTDEGREEREQGROUPS
					SET INCLUDED = -1
					FROM EA7STUDENTDEGREEREQGROUPS SDRG
					WHERE SDRG.EA7STUDENTDEGREEREQSID =  @EA7STUDENTDEGREEREQSID

					UPDATE EA7STUDENTDEGREEREQGROUPS
					SET INCLUDED = 0
					FROM EA7STUDENTDEGREEREQGROUPS
					WHERE EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND GROUPID IN
						(SELECT DISTINCT FILTERVALUES7.FILTERIDVALUE1
						  FROM EA7STUDENTDEGREEREQGROUPS SDRG
						  INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDRG.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
						  INNER JOIN EA7DEGREEREQGROUPS DRG ON SDR.EA7DEGREEREQUIREMENTSID = DRG.EA7DEGREEREQUIREMENTSID AND DRG.GROUPID = SDRG.GROUPID
						  INNER JOIN FILTERVALUES7 ON DRG.EA7DEGREEREQGROUPSID = FILTERVALUES7.PARENTID
						  WHERE FILTERVALUES7.INCLUDEOPTION = 2 AND FILTERVALUES7.RECORDTYPE = 1461 AND SDRG.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND SDRG.GROUPID = @GroupID)
				end
			end

			SET @NumberGroupsComplete = (Select Count(*) FROM EA7STUDENTDEGREEREQGROUPS SDRG
							 WHERE SDRG.STATUS = 2 AND SDRG.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND SDRG.INCLUDED = -1)
		
			if ((@NumberGroupsRequired = -1) AND (@NumberGroupsComplete > 0)) OR ((@NumberGroupsRequired > 0) And (@NumberGroupsComplete >= @NumberGroupsRequired))
			begin
				--get the number of exempt/waive/substitute credits we cannot use because its group cannot be used with an included group
				SET @NonUsableExemptWaiveCredits = (SELECT coalesce(sum(SDRG.WAIVEDCREDITS),0) + coalesce(sum(SDRG.EXEMPTEDCREDITS),0) + coalesce(sum(SDRG.SUBSTITUTEDCREDITS),0)
													FROM EA7STUDENTDEGREEREQGROUPS SDRG
								    				WHERE SDRG.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND SDRG.INCLUDED = 0)

				SET @CREDITSNEEDED = (SELECT coalesce(SDR.CREDITSREQUIRED,0) - (coalesce(sum(SDRC.CREDITSUSED),0) + coalesce(SDR.WAIVEDCREDITS,0) + coalesce(SDR.SUBSTITUTEDCREDITS,0) + coalesce(SDR.EXEMPTEDCREDITS,0) - @NonUsableExemptWaiveCredits)
							FROM EA7STUDENTDEGREEREQS SDR
							INNER JOIN EA7STUDENTDEGREEREQGROUPS SDRG ON SDR.EA7STUDENTDEGREEREQSID = SDRG.EA7STUDENTDEGREEREQSID
							INNER JOIN EA7STUDENTDEGREEREQCOURSES SDRC ON SDRG.EA7STUDENTDEGREEREQSID = SDRC.EA7STUDENTDEGREEREQSID AND SDRG.GROUPID = SDRC.GROUPID
							WHERE SDR.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND SDRG.STATUS = 2 AND SDRG.INCLUDED = -1 AND SDRC.STATUS = 2
							GROUP BY SDR.CREDITSREQUIRED, SDR.WAIVEDCREDITS, SDR.SUBSTITUTEDCREDITS, SDR.EXEMPTEDCREDITS)



				if (@CREDITSNEEDED > 0) AND (@NumberGroupsRequired > 0)
				begin
					--add courses from incomplete groups that can be included in order to meet credits needed
					while (SELECT coalesce(sum(SDRC.CreditsUsed),0) FROM @ExtraCourses EC INNER JOIN EA7STUDENTDEGREEREQCOURSES SDRC ON EC.EA7STUDENTDEGREEREQCOURSESID = SDRC.EA7STUDENTDEGREEREQCOURSESID) < @CREDITSNEEDED AND
						exists(SELECT SDRC.EA7STUDENTDEGREEREQCOURSESID
							FROM EA7STUDENTDEGREEREQCOURSES SDRC
							INNER JOIN EA7STUDENTDEGREEREQGROUPS SDRG ON SDRC.EA7STUDENTDEGREEREQSID = SDRG.EA7STUDENTDEGREEREQSID AND SDRC.GROUPID = SDRG.GROUPID
							INNER JOIN @STUDENTDEGREEINFO SDI ON SDRC.EA7COURSESID = SDI.EA7COURSESID AND SDRC.EA7STUDENTDEGREEREQSID = SDI.EA7STUDENTDEGREEREQSID
							LEFT OUTER JOIN @ExtraCourses EC ON EC.EA7STUDENTDEGREEREQCOURSESID = SDRC.EA7STUDENTDEGREEREQCOURSESID
							WHERE SDRG.STATUS <> 2 AND SDRC.STATUS = 2 AND SDRG.INCLUDED = -1 AND SDRC.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND EC.ExtraCoursesID IS NULL)
					begin
						INSERT INTO @ExtraCourses(EA7STUDENTDEGREEREQCOURSESID)
						SELECT TOP 1 SDRC.EA7STUDENTDEGREEREQCOURSESID
						FROM EA7STUDENTDEGREEREQCOURSES SDRC
						INNER JOIN EA7STUDENTDEGREEREQGROUPS SDRG ON SDRC.EA7STUDENTDEGREEREQSID = SDRG.EA7STUDENTDEGREEREQSID AND SDRC.GROUPID = SDRG.GROUPID
						INNER JOIN @STUDENTDEGREEINFO SDI ON SDRC.EA7COURSESID = SDI.EA7COURSESID AND SDRC.EA7STUDENTDEGREEREQSID = SDI.EA7STUDENTDEGREEREQSID
						LEFT OUTER JOIN @ExtraCourses EC ON EC.EA7STUDENTDEGREEREQCOURSESID = SDRC.EA7STUDENTDEGREEREQCOURSESID
						WHERE SDRG.STATUS <> 2 AND SDRC.STATUS = 2 AND SDRC.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND EC.ExtraCoursesID IS NULL
						ORDER BY SDI.STUDENTDEGREEINFOID
					end

					--update degree req status
					--ScottKa 02/05/2009 CR314555-013009
					UPDATE EA7STUDENTDEGREEREQS
					SET STATUS = 2
					FROM EA7STUDENTDEGREEREQS 
					WHERE EA7STUDENTDEGREEREQSID IN
						(SELECT SDR.EA7STUDENTDEGREEREQSID
						 FROM EA7STUDENTDEGREEREQS SDR
						 INNER JOIN EA7STUDENTDEGREEREQGROUPS SDRG ON SDR.EA7STUDENTDEGREEREQSID = SDRG.EA7STUDENTDEGREEREQSID
						 LEFT OUTER JOIN EA7STUDENTDEGREEREQCOURSES SDRC ON SDR.EA7STUDENTDEGREEREQSID = SDRC.EA7STUDENTDEGREEREQSID AND SDRC.GROUPID = SDRG.GROUPID
						 LEFT OUTER JOIN EA7STUDENTDEGREQCOURSELINKS SDRCL ON SDRC.EA7STUDENTDEGREEREQCOURSESID = SDRCL.EA7STUDENTDEGREEREQCOURSESID
						 LEFT OUTER JOIN @ExtraCourses EC ON SDRC.EA7STUDENTDEGREEREQCOURSESID = EC.EA7STUDENTDEGREEREQCOURSESID
						 WHERE SDR.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND (SDRG.STATUS = 2 Or (SDRG.STATUS <> 2 AND EC.ExtraCoursesID IS NOT NULL)) AND SDRCL.STATUS = 2 AND SDRG.INCLUDED = -1
						 GROUP BY SDR.EA7STUDENTDEGREEREQSID, SDR.CREDITSREQUIRED, SDR.WAIVEDCREDITS, SDR.SUBSTITUTEDCREDITS, SDR.EXEMPTEDCREDITS
						 HAVING (coalesce(SUM(SDRCL.CREDITSUSED),0) + COALESCE(SDR.WAIVEDCREDITS,0) + COALESCE(SDR.SUBSTITUTEDCREDITS,0) + COALESCE(SDR.EXEMPTEDCREDITS,0) - @NonUsableExemptWaiveCredits) >= COALESCE(SDR.CREDITSREQUIRED,0))
				end
				else if (@CREDITSNEEDED <= 0)
					--ScottKa 05/29/2009 7.82 Zero Credit Classes: "Excluding Completed Using": 2-Completed tasks
					UPDATE EA7STUDENTDEGREEREQS
					SET STATUS = 2
					FROM EA7STUDENTDEGREEREQS SDR
					INNER JOIN EA7DEGREEREQUIREMENTS DR ON SDR.EA7DEGREEREQUIREMENTSID = DR.EA7DEGREEREQUIREMENTSID
					WHERE SDR.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND DR.COMPLETEDUSING = 1

				if (Select STATUS FROM EA7STUDENTDEGREEREQS WHERE EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID) = 2
				begin
					--set included status of non-complete groups
					UPDATE EA7STUDENTDEGREEREQGROUPS
					SET INCLUDED = 0
					FROM EA7STUDENTDEGREEREQGROUPS SDRG
					WHERE SDRG.STATUS <> 2 AND SDRG.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID


					--CR343238-051812 When checking included groups, join to specific groups to not create duplicates, and take into account that SUM(CREDITSUSED) is counting instances other than the current course, so use greater than equal to since current course will be extra if requirements are already met
					--remove all the unused courses from the requirement
					while exists(SELECT SDRC.EA7STUDENTDEGREEREQCOURSESID 
							 FROM EA7STUDENTDEGREEREQCOURSES SDRC
							 INNER JOIN EA7STUDENTDEGREEREQGROUPS SDRG ON SDRC.EA7STUDENTDEGREEREQSID = SDRG.EA7STUDENTDEGREEREQSID AND SDRC.GROUPID = SDRG.GROUPID
							 LEFT OUTER JOIN @ExtraCourses EC ON SDRC.EA7STUDENTDEGREEREQCOURSESID = EC.EA7STUDENTDEGREEREQCOURSESID
							 WHERE SDRG.INCLUDED = 0 AND SDRC.EA7STUDENTDEGREEREQSID =  @EA7STUDENTDEGREEREQSID AND EC.ExtraCoursesID IS NULL) 
					or exists(SELECT SDRC.EA7STUDENTDEGREEREQCOURSESID 
						  FROM EA7STUDENTDEGREEREQCOURSES SDRC 
						  INNER JOIN EA7STUDENTDEGREEREQGROUPS SDRG ON SDRC.EA7STUDENTDEGREEREQSID = SDRG.EA7STUDENTDEGREEREQSID AND SDRC.GROUPID = SDRG.GROUPID
						  INNER JOIN @StudentDegreeInfo SDI ON SDRC.EA7COURSESID = SDI.EA7COURSESID AND SDRC.EA7STUDENTDEGREEREQSID = SDI.EA7STUDENTDEGREEREQSID
						  WHERE SDRG.INCLUDED = -1 AND SDRC.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND
						  (SELECT COALESCE(SUM(SDRC2.CREDITSUSED),0) FROM EA7STUDENTDEGREEREQCOURSES SDRC2 INNER JOIN @StudentDegreeInfo SDI2 ON SDRC2.EA7COURSESID = SDI2.EA7COURSESID AND SDRC2.EA7STUDENTDEGREEREQSID = SDI2.EA7STUDENTDEGREEREQSID WHERE SDRC2.EA7STUDENTDEGREEREQSID = SDRC.EA7STUDENTDEGREEREQSID AND SDRC2.GROUPID = SDRC.GROUPID AND SDI2.STUDENTDEGREEINFOID < SDI.STUDENTDEGREEINFOID AND SDRC2.STATUS = 2) >= SDRG.CREDITSREQUIRED AND 
						  (SELECT COALESCE(SUM(SDRC3.CREDITSUSED),0) FROM EA7STUDENTDEGREEREQCOURSES SDRC3 INNER JOIN EA7STUDENTDEGREEREQGROUPS SDRG2 ON SDRC3.EA7STUDENTDEGREEREQSID = SDRG2.EA7STUDENTDEGREEREQSID AND SDRC3.GROUPID = SDRG2.GROUPID WHERE SDRG2.INCLUDED = -1 AND SDRC3.EA7STUDENTDEGREEREQSID = SDRC.EA7STUDENTDEGREEREQSID AND SDRC3.EA7STUDENTDEGREEREQCOURSESID <> SDRC.EA7STUDENTDEGREEREQCOURSESID AND SDRC3.STATUS = 2) >= SDI.CREDITSREQUIREDTO)					
					begin
						SET @ExtraCourseID = 0

						--CR308689-092208 Set @ExtraCourseID to 0 if returns null so it doesn't stay in an infinite loop
						--remove any courses that are not in a complete group and are not being used to meet credits required
						SET @ExtraCourseID = coalesce((SELECT TOP 1 SDRC.EA7STUDENTDEGREEREQCOURSESID
									  FROM EA7STUDENTDEGREEREQCOURSES SDRC
										   INNER JOIN EA7STUDENTDEGREEREQGROUPS SDRG ON SDRC.EA7STUDENTDEGREEREQSID = SDRG.EA7STUDENTDEGREEREQSID AND SDRC.GROUPID = SDRG.GROUPID
									  LEFT OUTER JOIN @ExtraCourses EC ON SDRC.EA7STUDENTDEGREEREQCOURSESID = EC.EA7STUDENTDEGREEREQCOURSESID
										   WHERE SDRG.INCLUDED = 0 AND SDRC.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND EC.ExtraCoursesID IS NULL), 0)

						if @ExtraCourseID = 0
							--CR343238-051812 When checking included groups, join to specific groups to not create duplicates, and take into account that SUM(CREDITSUSED) is counting instances other than the current course, so use greater than equal to since current course will be extra if requirements are already met
							--remove extra courses from included groups
							SET @ExtraCourseID = (SELECT TOP 1 SDRC.EA7STUDENTDEGREEREQCOURSESID 
										FROM EA7STUDENTDEGREEREQCOURSES SDRC 
										  INNER JOIN EA7STUDENTDEGREEREQGROUPS SDRG ON SDRC.EA7STUDENTDEGREEREQSID = SDRG.EA7STUDENTDEGREEREQSID AND SDRC.GROUPID = SDRG.GROUPID
										  INNER JOIN @StudentDegreeInfo SDI ON SDRC.EA7COURSESID = SDI.EA7COURSESID AND SDRC.EA7STUDENTDEGREEREQSID = SDI.EA7STUDENTDEGREEREQSID
										  WHERE SDRG.INCLUDED = -1 AND SDRC.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND
										(SELECT COALESCE(SUM(SDRC2.CREDITSUSED),0) FROM EA7STUDENTDEGREEREQCOURSES SDRC2 INNER JOIN @StudentDegreeInfo SDI2 ON SDRC2.EA7COURSESID = SDI2.EA7COURSESID AND SDRC2.EA7STUDENTDEGREEREQSID = SDI2.EA7STUDENTDEGREEREQSID WHERE SDRC2.EA7STUDENTDEGREEREQSID = SDRC.EA7STUDENTDEGREEREQSID AND SDRC2.GROUPID = SDRC.GROUPID AND SDI2.STUDENTDEGREEINFOID < SDI.STUDENTDEGREEINFOID AND SDRC2.STATUS = 2) >= SDRG.CREDITSREQUIRED AND 
										(SELECT COALESCE(SUM(SDRC3.CREDITSUSED),0) FROM EA7STUDENTDEGREEREQCOURSES SDRC3 INNER JOIN EA7STUDENTDEGREEREQGROUPS SDRG2 ON SDRC3.EA7STUDENTDEGREEREQSID = SDRG2.EA7STUDENTDEGREEREQSID AND SDRC3.GROUPID = SDRG2.GROUPID WHERE SDRG2.INCLUDED = -1 AND SDRC3.EA7STUDENTDEGREEREQSID = SDRC.EA7STUDENTDEGREEREQSID AND SDRC3.EA7STUDENTDEGREEREQCOURSESID <> SDRC.EA7STUDENTDEGREEREQCOURSESID AND SDRC3.STATUs = 2) >= SDI.CREDITSREQUIREDTO
										ORDER BY SDI.STUDENTDEGREEINFOID DESC)

						SET @CanSatisfy = (Select SDI.CANSATISFY FROM EA7STUDENTDEGREEREQCOURSES SDRC INNER JOIN @STUDENTDEGREEINFO SDI ON SDI.EA7STUDENTDEGREEREQSID = SDRC.EA7STUDENTDEGREEREQSID AND SDI.EA7DEGREEREQCOURSESID = SDRC.EA7DEGREEREQCOURSESID WHERE SDRC.EA7STUDENTDEGREEREQCOURSESID = @ExtraCourseID)

						--since we are no longer using this course we need to set the processed status of courses that we
						--did not process due to the CanSatisfy field back to 0, provided no other course has set those same
						--courses to not be processed, so they can be processed now that this course is not being used; 
						--however, if the req that course belongs to is already complete we do not need to bother
						if (@CanSatisfy = 2) Or (@CanSatisfy = 3) Or (@CanSatisfy = 4)
						begin
							UPDATE @TempStudentDegTable
							SET PROCESSED = 0
							FROM @TempStudentDegTable TSDT
							INNER JOIN @STUDENTDEGREEINFO SDI ON TSDT.STUDENTDEGREEINFOID = SDI.STUDENTDEGREEINFOID
							INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDI.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
							INNER JOIN @CanSatisfyLinks CSL ON TSDT.STUDENTDEGREEINFOID = CSL.STUDENTDEGREEINFOID
							WHERE SDR.STATUS <> 2 AND CSL.EA7STUDENTDEGREEREQCOURSESID = @ExtraCourseID AND (SELECT COUNT(*) FROM @CanSatisfyLinks CSL2 WHERE CSL2.STUDENTDEGREEINFOID = CSL.STUDENTDEGREEINFOID) = 1

							DELETE FROM @CanSatisfyLinks WHERE EA7STUDENTDEGREEREQCOURSESID = @ExtraCourseID
							
							--GPB ~ CR317548-040609 ~ Need to drop this when we drop it from EA7STUDENTDEGREEREQCOURSES as well...
							DELETE FROM @TempCachedCourseLinks WHERE EA7STUDENTDEGREEREQCOURSESID = @ExtraCourseID
						end

						--update number of credits applied towards group of extra course
						UPDATE EA7STUDENTDEGREEREQGROUPS						
						--CR307580-090408 since we only applied it to that group, we should only take it away from that group
						SET CREDITSAPPLIED = COALESCE(CREDITSAPPLIED, 0) - SDRC.CREDITSUSED
															 -- - (SELECT SDRC.CREDITSUSED FROM EA7STUDENTDEGREEREQCOURSES SDRC WHERE SDRC.EA7STUDENTDEGREEREQCOURSESID = @ExtraCourseID)												
						FROM EA7STUDENTDEGREEREQGROUPS SDRG
						INNER JOIN EA7STUDENTDEGREEREQCOURSES SDRC ON SDRC.EA7STUDENTDEGREEREQSID = SDRG.EA7STUDENTDEGREEREQSID AND SDRC.EA7STUDENTDEGREEREQCOURSESID = @ExtraCourseID 
						INNER JOIN EA7DEGREEREQCOURSES DRC ON SDRC.EA7COURSESID = DRC.EA7COURSESID AND DRC.GROUPID = SDRG.GROUPID
						
						DELETE FROM EA7STUDENTDEGREEREQCOURSES WHERE EA7STUDENTDEGREEREQCOURSESID = @ExtraCourseID
					end
					
					--when we have extra credits due to group functionality remove those credits from a general elective requirement
					set @CREDITSUSED = 0

					SELECT @CREDITSUSED = (SELECT coalesce(sum(SDRC.CREDITSUSED),0) + coalesce(SDR.WAIVEDCREDITS,0) + coalesce(SDR.SUBSTITUTEDCREDITS,0) + coalesce(SDR.EXEMPTEDCREDITS,0) - @NonUsableExemptWaiveCredits
								   FROM EA7STUDENTDEGREEREQS SDR
								   LEFT OUTER JOIN EA7STUDENTDEGREEREQCOURSES SDRC ON SDR.EA7STUDENTDEGREEREQSID = SDRC.EA7STUDENTDEGREEREQSID
								   WHERE SDR.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID AND SDRC.STATUS = 2
								   GROUP BY SDR.WAIVEDCREDITS, SDR.SUBSTITUTEDCREDITS, SDR.EXEMPTEDCREDITS)

					SET @ExtraCredits = @CREDITSUSED - @CREDITSREQUIREDTO

					--take into account exempt/substitute credits that may not have been applied due to there not being enough credits in the general elective requirements
					SET @UnappliedExemptSubstituteCredits = (SELECT EST.TotalUnappliedCredits
											FROM @ExemptedSubstitutedTotals EST
											WHERE EST.EA7STUDENTDEGREESID = @EA7STUDENTDEGREESID)

					if @UnappliedExemptSubstituteCredits > 0
					begin
						UPDATE @ExemptedSubstitutedTotals
						SET TotalUnappliedCredits = Case When @ExtraCredits > TotalUnappliedCredits then 0 Else TotalUnappliedCredits - @ExtraCredits end

						SET @ExtraCredits = @ExtraCredits - @UnappliedExemptSubstituteCredits
					end

					while @ExtraCredits > 0
					begin
						SET @GEReqID = (Select Top 1 CES.CalculateExemptionsSubstitutionsID
								FROM @CalculateExemptionsSubstitutions CES
								INNER JOIN EA7STUDENTDEGREEREQS SDR ON CES.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
								WHERE SDR.STATUS <> 2 AND CES.CREDITSREQUIRED > 0)

						SET @GECreditsRequired = (SELECT CES.CREDITSREQUIRED FROM @CalculateExemptionsSubstitutions CES WHERE CES.CalculateExemptionsSubstitutionsID = @GEReqID)

						UPDATE @CalculateExemptionsSubstitutions
						SET CREDITSREQUIRED = CASE WHEN @ExtraCredits > CES.CREDITSREQUIRED THEN 0 ELSE CES.CREDITSREQUIRED - @ExtraCredits END
						FROM @CalculateExemptionsSubstitutions CES
						WHERE CES.CalculateExemptionsSubstitutionsID = @GEReqID

						UPDATE @StudentDegreeInfo
						SET CREDITSREQUIRED = CES.CREDITSREQUIRED
						FROM @StudentDegreeInfo SDI 
						INNER JOIN @CalculateExemptionsSubstitutions CES ON SDI.EA7STUDENTDEGREEREQSID = CES.EA7STUDENTDEGREEREQSID
						WHERE CES.CalculateExemptionsSubstitutionsID = @GEReqID

						UPDATE EA7STUDENTDEGREEREQS
						SET CREDITSREQUIRED = CES.CREDITSREQUIRED
						FROM EA7STUDENTDEGREEREQS SDR
						INNER JOIN @CalculateExemptionsSubstitutions CES ON SDR.EA7STUDENTDEGREEREQSID = CES.EA7STUDENTDEGREEREQSID
						WHERE CES.CalculateExemptionsSubstitutionsID = @GEReqID

						SET @ExtraCredits = @ExtraCredits - @GECreditsRequired 
					end

					--if the requirement is complete we do not need to process any other courses in the requirement
					UPDATE @TempStudentDegTable
					SET PROCESSED = -1
					FROM @TempStudentDegTable TSDT
					INNER JOIN @STUDENTDEGREEINFO SDI ON TSDT.STUDENTDEGREEINFOID = SDI.STUDENTDEGREEINFOID
					WHERE SDI.EA7STUDENTDEGREEREQSID = @EA7STUDENTDEGREEREQSID
				end				

				delete
				from @ExtraCourses
			end
		end
	END

	WHILE EXISTS (SELECT EA7STUDENTDEGREESID FROM @ExemptedSubstitutedTotals WHERE PROCESSED = 0)
	BEGIN
		Declare @TotalUnappliedCredits numeric(19,4)

		SELECT TOP 1 @EA7StudentDegreesID = EA7STUDENTDEGREESID, @TotalUnappliedCredits = TOTALUNAPPLIEDCREDITS
		FROM @ExemptedSubstitutedTotals
		WHERE PROCESSED = 0

		exec dbo.EA7STUDENT_PROCESSDEGREES_UPDATESUBSTITUTECREDITS @EA7StudentDegreesID, @TotalUnappliedCredits, @bIsWhatIf

		UPDATE @ExemptedSubstitutedTotals
		SET PROCESSED = -1
		FROM @ExemptedSubstitutedTotals EST
		WHERE EST.EA7STUDENTDEGREESID = @EA7StudentDegreesID
	END

	SET @CREDITSUSED = 0

	--now that we're done with the individual courses, get credits used for later
	--05/07/2007 SAM CR274460-050307 & CR274547-050407 - added status field to ea7studentdegreqcourselinks
	SELECT @CREDITSUSED = coalesce( SUM(coalesce(sdrcl.CREDITSUSED,0)),0)
	FROM EA7STUDENTDEGREQCOURSELINKS sdrcl 
	INNER JOIN EA7STUDENTDEGREEREQCOURSES SDRC ON sdrcl.EA7STUDENTDEGREEREQCOURSESID = SDRC.EA7STUDENTDEGREEREQCOURSESID
	INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDRC.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
	INNER JOIN EA7STUDENTDEGREES SD ON SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID
	WHERE SD.EA7STUDENTSID=@EA7STUDENTSID AND sdrcl.status = 2 AND SD.COMPLETEDON IS NULL AND SD.ISWHATIF = @bIsWhatIf	--2 = complete, only count complete ones here

	--can't include this above because multiple records in EA7STUDENTDEGREEREQCOURSES will throw off our sum
	--JAK - only include credits from subs that are not used in other requirements since we do not want to count those credits twice
	--CR307890-090908 When the requirement is complete due to substitution, it never gets a studentreqcourselink, so the creditsused from ea7studentdegreereqsubs did not get counted
	SELECT @CREDITSUSED = @CREDITSUSED + coalesce(SUM(SDR.WAIVEDCREDITS), 0) + coalesce(SUM(CASE SDR.STATUS WHEN 2 THEN SDR.SUBSTITUTEDCREDITS ELSE 0 END), 0)
				+ (SELECT coalesce(SUM(CASE SDRS.USEINOTHERREQUIREMENTS WHEN 0 THEN SDRS.CREDITSUSED ELSE 0 END),0)
				   FROM EA7STUDENTDEGREEREQSUBS SDRS
				   INNER JOIN EA7STUDENTDEGREQCOURSELINKS SDRCL ON SDRS.EA7STUDENTCOURSESID = SDRCL.EA7STUDENTCOURSESID
				   WHERE SDRS.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID)
	FROM EA7STUDENTDEGREEREQS SDR
	INNER JOIN EA7STUDENTDEGREES SD ON SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID
	WHERE SD.EA7STUDENTSID=@EA7STUDENTSID AND SD.COMPLETEDON IS NULL AND SD.ISWHATIF = @bIsWhatIf	
	GROUP BY SDR.EA7STUDENTDEGREEREQSID

	SET @OTHERCREDITSUSED = 0
	
	--ScottKa 08/11/2009 CR323466-081009
	SELECT @OTHERCREDITSUSED = CASE WHEN (COALESCE(SUM(SG.CREDITAWARDED),0) > MAX(SDRC.CREDITSUSED)) THEN MAX(SDRC.CREDITSUSED) ELSE COALESCE(SUM(SG.CREDITAWARDED),0) END
	FROM EA7STUDENTDEGREQCOURSELINKS SDRCL
	INNER JOIN EA7STUDENTDEGREEREQCOURSES SDRC ON SDRCL.EA7STUDENTDEGREEREQCOURSESID = SDRC.EA7STUDENTDEGREEREQCOURSESID
	INNER JOIN EA7STUDENTDEGREEREQS SDR ON SDRC.EA7STUDENTDEGREEREQSID = SDR.EA7STUDENTDEGREEREQSID
	INNER JOIN EA7STUDENTDEGREES SD ON SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID
	INNER JOIN EA7STUDENTCOURSES SC ON SDRCL.EA7STUDENTCOURSESID = SC.EA7STUDENTCOURSESID
	INNER JOIN EA7STUDENTGRADES SG ON SC.EA7STUDENTCOURSESID = SG.EA7STUDENTCOURSESID
	WHERE SC.ISOTHERCOURSE = -1 and SC.EA7STUDENTSID = @EA7STUDENTSID AND SD.ISWHATIF = @bIsWhatIf	

	--Update Majors, Minors, Concentrations, and Options	
	UPDATE EA7STUDENTDEGREES
	SET STATUS = 2
	FROM EA7STUDENTDEGREES SD 
	INNER JOIN EA7DEGREES D ON SD.EA7DEGREESID = D.EA7DEGREESID
	WHERE SD.EA7STUDENTSID = @EA7STUDENTSID AND SD.COMPLETEDON IS NULL AND SD.ISWHATIF = @bIsWhatIf	
	AND (EXISTS(SELECT APP.EA7STUDENTDEGREEAPPROVALSID	--at least one approval set to yes
			FROM EA7STUDENTDEGREEAPPROVALS APP
			WHERE COALESCE(APPROVES,0) = -1 AND APP.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID) OR
			NOT EXISTS (SELECT APP2.EA7STUDENTDEGREEAPPROVALSID		--or no approval information at ALL
			FROM EA7STUDENTDEGREEAPPROVALS APP2
			WHERE APP2.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID))
	AND NOT EXISTS(SELECT SDR.EA7STUDENTDEGREEREQSID
			FROM EA7STUDENTDEGREEREQS SDR
			WHERE SDR.STATUS = 1 AND SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID)
	AND SD.DEGREETYPE IN (2,3,4,5)
	AND (COALESCE(SD.DEGREEGPA,0) >= COALESCE(D.MINGPA,0) OR D.MINGPA IS NULL)

	--Update Degrees - 
	UPDATE EA7STUDENTDEGREES
	SET STATUS = 2
	FROM EA7STUDENTDEGREES SD 
	INNER JOIN EA7DEGREES D ON SD.EA7DEGREESID = D.EA7DEGREESID
	WHERE SD.EA7STUDENTSID = @EA7STUDENTSID AND SD.COMPLETEDON IS NULL AND SD.ISWHATIF = @bIsWhatIf	
	AND (EXISTS(SELECT APP.EA7STUDENTDEGREEAPPROVALSID	--at least one approval set to yes
			FROM EA7STUDENTDEGREEAPPROVALS APP
			WHERE COALESCE(APPROVES,0) = -1 AND APP.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID) OR
			NOT EXISTS (SELECT APP2.EA7STUDENTDEGREEAPPROVALSID		--or no approval information at ALL
			FROM EA7STUDENTDEGREEAPPROVALS APP2
			WHERE APP2.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID))
	AND NOT EXISTS(SELECT SDR.EA7STUDENTDEGREEREQSID
			FROM EA7STUDENTDEGREEREQS SDR
			WHERE SDR.STATUS = 1 AND SDR.EA7STUDENTDEGREESID = SD.EA7STUDENTDEGREESID)
	--jkl 09.17.08 (CR308444-091708) alias below was the same as main table; changed to be distinct
	AND NOT EXISTS(SELECT SDM.EA7STUDENTDEGREESID	--check for incompete majors/minors
			FROM EA7STUDENTDEGREES SDM
			WHERE SDM.PARENTDEGREEID = SD.EA7STUDENTDEGREESID AND SDM.STATUS = 1)
	AND NOT EXISTS(SELECT CONS.EA7STUDENTDEGREESID	--check for incomplete concentrations/options
			FROM EA7STUDENTDEGREES CONS 
			INNER JOIN EA7STUDENTDEGREES MAJORS ON CONS.PARENTDEGREEID = MAJORS.EA7STUDENTDEGREESID
			WHERE MAJORS.PARENTDEGREEID = SD.EA7STUDENTDEGREESID and CONS.STATUS = 1)
	AND SD.DEGREETYPE = (1)
	AND coalesce(@CREDITSUSED,0) >= COALESCE(D.MINIMUMCREDITS,coalesce(dbo.sf_EA7DEGREES_GETTOTALCREDITSFORDEGREE(D.EA7DEGREESID),0),0)
	AND (coalesce(@CREDITSUSED,0) - coalesce(@OTHERCREDITSUSED,0)) >= COALESCE(D.INHOUSECREDITS,0)
	
	UPDATE EA7STUDENTDEGREES
	SET PREVIOUSSTATUS = NULL
	WHERE EA7STUDENTSID = @EA7STUDENTSID AND COMPLETEDON IS NULL AND EA7STUDENTDEGREES.ISWHATIF = @bIsWhatIf
	
	-- based on dbrevision feedback, does not need to join on students
	set @tint_IsCollege = 0
	if exists (select *
		from dbo.GeneralInfo7 g --inner join dbo.EA7Students s on g.GeneralInfo7ID = s.EA7GeneralInfoID
		where --s.EA7StudentsID = @EA7STUDENTSID and 
		((g.CommonInstalledProducts & 128) = 128))  --bbAFNsys_College = 128 //supersedes bbROsys_College 
	set @tint_IsCollege = 1
	
	--JAK CR308737-092308 9/23/08
	UPDATE EA7STUDENTDEGREES
	SET COMPLETEDON = getdate()
	WHERE STATUS = 2 AND EA7STUDENTSID = @EA7STUDENTSID AND COMPLETEDON IS NULL AND DEGREETYPE = 1 and (@tint_IsCollege = 1) AND ISWHATIF = @bIsWhatIf	

	--One more thing...
	UPDATE EA7STUDENTS
	SET PROCESSDEGREES = 0
	WHERE EA7STUDENTSID = @EA7STUDENTSID
	COMMIT TRANSACTION
	
	End TRY
	
	BEGIN CATCH
	ROLLBACK
	END CATCH
END      
      
GO


