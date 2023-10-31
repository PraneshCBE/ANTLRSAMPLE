parser grammar Java8Parser;

options {
    tokenVocab=Java8Lexer;
}
/*
 * Productions from §3 (Lexical Structure)
 */

literal
	:	IntegerLiteral
	|	FloatingPointLiteral
	|	BooleanLiteral
	|	CharacterLiteral
	|	StringLiteral
	|	NullLiteral
	;

/*
 * Productions from §4 (Types, Values, and Variables)
 */

primitiveType
    :   annotationZeroOrMore numericTypeOrBoolean
    ;

numericTypeOrBoolean
    :   numericType
    |   'boolean'
    ;

numericType
	:	integralType
	|	floatingPointType
	;

integralType
	:	'byte'
	|	'short'
	|	'int'
	|	'long'
	|	'char'
	;

floatingPointType
	:	'float'
	|	'double'
	;

referenceType
	:	classOrInterfaceType
	|	typeVariable
	|	arrayType
	;

classOrInterfaceType
	:	(	classType_lfno_classOrInterfaceType
		|	interfaceType_lfno_classOrInterfaceType
		)
		(	classType_lf_classOrInterfaceType
		|	interfaceType_lf_classOrInterfaceType
		)*
	;

classType
    :   annotationZeroOrMore Identifier typeArgumentsZeroOrOne
    |   classOrInterfaceType '.' annotationZeroOrMore Identifier typeArgumentsZeroOrOne
    ;

annotationZeroOrMore
    :   annotation annotationZeroOrMore
    |
    ;

typeArgumentsZeroOrOne
    :   typeArguments
    |
    ;


classType_lf_classOrInterfaceType
	:	'.' annotationZeroOrMore Identifier typeArgumentsZeroOrOne
	;

classType_lfno_classOrInterfaceType
	:	annotationZeroOrMore Identifier typeArgumentsZeroOrOne
	;

interfaceType
	:	classType
	;

interfaceType_lf_classOrInterfaceType
	:	classType_lf_classOrInterfaceType
	;

interfaceType_lfno_classOrInterfaceType
	:	classType_lfno_classOrInterfaceType
	;

typeVariable
    :   annotationZeroOrMore Identifier
    ;

arrayType
	:	primitiveType dims
	|	classOrInterfaceType dims
	|	typeVariable dims
	;

dims
    :   annotationZeroOrMore '[' ']' dimsZeroOrMore
    ;

dimsZeroOrMore
    :   annotationZeroOrMore '[' ']' dimsZeroOrMore
    |
    ;

typeParameter
    :   typeParameterModifierZeroOrMore Identifier typeBoundZeroOrOne
    ;

typeParameterModifierZeroOrMore
    :   typeParameterModifier typeParameterModifierZeroOrMore
    |
    ;

typeBoundZeroOrOne
    :   typeBound
    |
    ;


typeParameterModifier
	:	annotation
	;

typeBound
    :   'extends' typeVariable
    |   'extends' classOrInterfaceType additionalBoundZeroOrMore
    ;

additionalBoundZeroOrMore
    :   additionalBound additionalBoundZeroOrMore
    |
    ;

additionalBound
	:	'&' interfaceType
	;

typeArguments
	:	'<' typeArgumentList '>'
	;

typeArgumentList
	:	typeArgument typeArgumentListZeroOrMore
	;
typeArgumentListZeroOrMore
	:	',' typeArgument typeArgumentListZeroOrMore
	|
	;

typeArgument
	:	referenceType
	|	wildcard
	;

wildcard
    :   annotationZeroOrMore '?' wildcardBoundsZeroOrOne
    ;

wildcardBoundsZeroOrOne
    :   wildcardBounds
    |
    ;


wildcardBounds
	:	'extends' referenceType
	|	'super' referenceType
	;

/*
 * Productions from §6 (Names)
 */

packageName
	:	Identifier
	|	packageName '.' Identifier
	;

typeName
	:	Identifier
	|	packageOrTypeName '.' Identifier
	;

packageOrTypeName
	:	Identifier
	|	packageOrTypeName '.' Identifier
	;

expressionName
	:	Identifier
	|	ambiguousName '.' Identifier
	;

methodName
	:	Identifier
	;

ambiguousName
	:	Identifier
	|	ambiguousName '.' Identifier
	;

/*
 * Productions from §7 (Packages)
 */

compilationUnit
    :   packageDeclarationZeroOrOne importDeclarationZeroOrMore typeDeclarationZeroOrMore EOF
    ;

packageDeclarationZeroOrOne
    :   packageDeclaration
    |
    ;

importDeclarationZeroOrMore
    :   importDeclaration importDeclarationZeroOrMore
    |
    ;

typeDeclarationZeroOrMore
    :   typeDeclaration typeDeclarationZeroOrMore
    |
    ;


packageDeclaration
	:	packageModifier* 'package' packageName ';'
	;

packageModifier
	:	annotation
	;

importDeclaration
	:	singleTypeImportDeclaration
	|	typeImportOnDemandDeclaration
	|	singleStaticImportDeclaration
	|	staticImportOnDemandDeclaration
	;

singleTypeImportDeclaration
	:	'import' typeName ';'
	;

typeImportOnDemandDeclaration
	:	'import' packageOrTypeName '.' '*' ';'
	;

singleStaticImportDeclaration
	:	'import' 'static' typeName '.' Identifier ';'
	;

staticImportOnDemandDeclaration
	:	'import' 'static' typeName '.' '*' ';'
	;

typeDeclaration
	:	classDeclaration
	|	interfaceDeclaration
	|	';'
	;

/*
 * Productions from §8 (Classes)
 */

classDeclaration
	:	normalClassDeclaration
	|	enumDeclaration
	;

normalClassDeclaration
    :   classModifierZeroOrMore 'class' Identifier typeParametersZeroOrOne superclassZeroOrOne superinterfacesZeroOrOne classBody
    ;

classModifierZeroOrMore
    :   classModifier classModifierZeroOrMore
    |
    ;

typeParametersZeroOrOne
    :   typeParameters
    |
    ;

superclassZeroOrOne
    :   superclass
    |
    ;

superinterfacesZeroOrOne
    :   superinterfaces
    |
    ;

classModifier
	:	annotation
	|	'public'
	|	'protected'
	|	'private'
	|	'abstract'
	|	'static'
	|	'final'
	|	'strictfp'
	;

typeParameters
	:	'<' typeParameterList '>'
	;

typeParameterList
    :   typeParameter typeParameterCommaSeparated
    ;

typeParameterCommaSeparated
    :   ',' typeParameter typeParameterCommaSeparated
    |
    ;


superclass
	:	'extends' classType
	;

superinterfaces
	:	'implements' interfaceTypeList
	;

interfaceTypeList
	:	interfaceType (',' interfaceType)*
	;

classBody
    :   '{' classBodyDeclarationZeroOrMore '}'
    ;

classBodyDeclarationZeroOrMore
    :   classBodyDeclaration classBodyDeclarationZeroOrMore
    |
    ;


classBodyDeclaration
	:	classMemberDeclaration
	|	instanceInitializer
	|	staticInitializer
	|	constructorDeclaration
	;

classMemberDeclaration
	:	fieldDeclaration
	|	methodDeclaration
	|	classDeclaration
	|	interfaceDeclaration
	|	';'
	;

fieldDeclaration
    :   fieldModifierZeroOrMore unannType variableDeclaratorList ';'
    ;

fieldModifierZeroOrMore
    :   fieldModifier fieldModifierZeroOrMore
    |
    ;

fieldModifier
	:	annotation
	|	'public'
	|	'protected'
	|	'private'
	|	'static'
	|	'final'
	|	'transient'
	|	'volatile'
	;

variableDeclaratorList
	:	variableDeclarator (',' variableDeclarator)*
	;

variableDeclarator
    :   variableDeclaratorId variableInitializerZeroOrOne
    ;


variableInitializerZeroOrOne
    :   '=' variableInitializer
    |
    ;

variableDeclaratorId
    :   Identifier dimsZeroOrOne
    ;

dimsZeroOrOne
    :   dims
    |
    ;


variableInitializer
	:	expression
	|	arrayInitializer
	;

unannType
	:	unannPrimitiveType
	|	unannReferenceType
	;

unannPrimitiveType
	:	numericType
	|	'boolean'
	;

unannReferenceType
	:	unannClassOrInterfaceType
	|	unannTypeVariable
	|	unannArrayType
	;

unannClassOrInterfaceType
	:	(	unannClassType_lfno_unannClassOrInterfaceType
		|	unannInterfaceType_lfno_unannClassOrInterfaceType
		)
		(	unannClassType_lf_unannClassOrInterfaceType
		|	unannInterfaceType_lf_unannClassOrInterfaceType
		)*
	;

unannClassType
	:	Identifier typeArgumentsZeroOrOne
	|	unannClassOrInterfaceType '.' annotationZeroOrMore Identifier typeArgumentsZeroOrOne
	;

unannClassType_lf_unannClassOrInterfaceType
	:	'.' annotationZeroOrMore Identifier typeArgumentsZeroOrOne
	;

unannClassType_lfno_unannClassOrInterfaceType
	:	Identifier typeArgumentsZeroOrOne
	;

unannInterfaceType
	:	unannClassType
	;

unannInterfaceType_lf_unannClassOrInterfaceType
	:	unannClassType_lf_unannClassOrInterfaceType
	;

unannInterfaceType_lfno_unannClassOrInterfaceType
	:	unannClassType_lfno_unannClassOrInterfaceType
	;

unannTypeVariable
	:	Identifier
	;

unannArrayType
	:	unannPrimitiveType dims
	|	unannClassOrInterfaceType dims
	|	unannTypeVariable dims
	;

methodDeclaration
    :   methodModifierZeroOrMore methodHeader methodBody
    ;

methodModifierZeroOrMore
    :   methodModifier methodModifierZeroOrMore
    |
    ;

methodModifier
	:	annotation
	|	'public'
	|	'protected'
	|	'private'
	|	'abstract'
	|	'static'
	|	'final'
	|	'synchronized'
	|	'native'
	|	'strictfp'
	;

methodHeader
    :   result methodDeclarator throwsZeroOrOne
    |   typeParameters annotationZeroOrMore result methodDeclarator throwsZeroOrOne
    ;

throwsZeroOrOne
    :   throws_
    |
    ;

result
	:	unannType
	|	'void'
	;

methodDeclarator
    :   Identifier '(' formalParameterListZeroOrOne ')' dimsZeroOrOne
    ;




formalParameterListZeroOrOne
    :   formalParameterList
    |
    ;

formalParameterList
	:	receiverParameter
	|	formalParameters ',' lastFormalParameter
	|	lastFormalParameter
	;

formalParameters
    :   formalParameter formalParameterListZeroOrMore
    |   receiverParameter receiverParameterListZeroOrMore
    ;

formalParameterListZeroOrMore
    :   ',' formalParameter formalParameterListZeroOrMore
    |
    ;

receiverParameterListZeroOrMore
    :   ',' formalParameter formalParameterListZeroOrMore
    |
    ;


formalParameter
    :   variableModifierZeroOrMore unannType variableDeclaratorId
    ;

variableModifierZeroOrMore
    :   variableModifier variableModifierZeroOrMore
    |
    ;


variableModifier
	:	annotation
	|	'final'
	;

lastFormalParameter
    :   lastFormalParameterWithEllipsis
    |   formalParameter
    ;

lastFormalParameterWithEllipsis
    :   variableModifierZeroOrMore unannType annotationZeroOrMore '...' variableDeclaratorId
    ;


receiverParameter
    :   annotationZeroOrMore unannType receiverIdentifier 'this'
    ;

receiverIdentifier
    :   Identifier '.'
    |   // Empty production for cases without an identifier
    ;

throws_
	:	'throws' exceptionTypeList
	;

exceptionTypeList
	:	exceptionType (',' exceptionType)*
	;

exceptionType
	:	classType
	|	typeVariable
	;

methodBody
	:	block
	|	';'
	;

instanceInitializer
	:	block
	;

staticInitializer
	:	'static' block
	;

constructorDeclaration
    :   constructorModifierZeroOrMore constructorDeclarator throwsQuestionMark constructorBody
    ;

constructorModifierZeroOrMore
    :   constructorModifier constructorModifierZeroOrMore
    |
    ;
throwsQuestionMark
    :   throws_
    |
    ;
constructorModifier
	:	annotation
	|	'public'
	|	'protected'
	|	'private'
	;

constructorDeclarator
    :   typeParametersZeroOrOne simpleTypeName '(' formalParameterListZeroOrOne ')'
    ;



simpleTypeName
	:	Identifier
	;

constructorBody
    :   '{' explicitConstructorInvocationZeroOrOne blockStatementsZeroOrOne '}'
    ;

explicitConstructorInvocationZeroOrOne
    :   explicitConstructorInvocation
    |
    ;

blockStatementsZeroOrOne
    :   blockStatements
    |
    ;


explicitConstructorInvocation
	:	typeArgumentsZeroOrOne 'this' '(' argumentListZeroOrOne ')' ';'
	|	typeArgumentsZeroOrOne 'super' '(' argumentListZeroOrOne ')' ';'
	|	expressionName '.' typeArgumentsZeroOrOne 'super' '(' argumentListZeroOrOne ')' ';'
	|	primary '.' typeArgumentsZeroOrOne 'super' '(' argumentListZeroOrOne ')' ';'
	;

enumDeclaration
    :   classModifierZeroOrMore 'enum' Identifier superinterfacesZeroOrOne enumBody
    ;



enumBody
    :   '{' enumConstantListZeroOrOne ','? enumBodyDeclarationsZeroOrOne '}'
    ;

enumConstantListZeroOrOne
    :   enumConstantList
    |
    ;

enumBodyDeclarationsZeroOrOne
    :   enumBodyDeclarations
    |
    ;
enumConstantList
    :   enumConstant enumConstantZeroOrMore
    ;

enumConstantZeroOrMore
    :   ',' enumConstant enumConstantZeroOrMore
    |
    ;


enumConstant
    :   enumConstantModifierZeroOrMore Identifier enumConstantArgumentsZeroOrOne classBodyZeroOrOne
    ;

enumConstantModifierZeroOrMore
    :   enumConstantModifier enumConstantModifierZeroOrMore
    |
    ;
enumConstantArgumentsZeroOrOne
    :   '(' argumentListZeroOrOne ')'
    |
    ;
argumentListZeroOrOne
    :   argumentList
    |
    ;
enumConstantModifier
	:	annotation
	;
	
classBodyZeroOrOne
    :   classBody
    |
    ;

enumBodyDeclarations
    :   ';' classBodyDeclarationZeroOrMore
    ;


/*
 * Productions from §9 (Interfaces)
 */

interfaceDeclaration
	:	normalInterfaceDeclaration
	|	annotationTypeDeclaration
	;

normalInterfaceDeclaration
    :   interfaceModifierZeroOrMore 'interface' Identifier typeParametersZeroOrOne extendsInterfacesZeroOrOne interfaceBody
    ;

interfaceModifierZeroOrMore
    :   interfaceModifier interfaceModifierZeroOrMore
    |
    ;


extendsInterfacesZeroOrOne
    :   extendsInterfaces
    |
    ;

interfaceModifier
	:	annotation
	|	'public'
	|	'protected'
	|	'private'
	|	'abstract'
	|	'static'
	|	'strictfp'
	;

extendsInterfaces
	:	'extends' interfaceTypeList
	;

interfaceBody
    :   '{' interfaceMemberDeclarationZeroOrMore '}'
    ;

interfaceMemberDeclarationZeroOrMore
    :   interfaceMemberDeclaration interfaceMemberDeclarationZeroOrMore
    |
    ;

interfaceMemberDeclaration
	:	constantDeclaration
	|	interfaceMethodDeclaration
	|	classDeclaration
	|	interfaceDeclaration
	|	';'
	;

constantDeclaration
    :   constantModifierZeroOrMore unannType variableDeclaratorList ';'
    ;

constantModifierZeroOrMore
    :   constantModifier constantModifierZeroOrMore
    |
    ;

constantModifier
	:	annotation
	|	'public'
	|	'static'
	|	'final'
	;

interfaceMethodDeclaration
    :   interfaceMethodModifierZeroOrMore methodHeader methodBody
    ;

interfaceMethodModifierZeroOrMore
    :   interfaceMethodModifier interfaceMethodModifierZeroOrMore
    |
    ;interfaceMethodModifier
	:	annotation
	|	'public'
	|	'abstract'
	|	'default'
	|	'static'
	|	'strictfp'
	;

annotationTypeDeclaration
    :   interfaceModifierZeroOrMore '@' 'interface' Identifier annotationTypeBody
    ;

annotationTypeBody
    :   '{' annotationTypeMemberDeclarationZeroOrMore '}'
    ;

annotationTypeMemberDeclarationZeroOrMore
    :   annotationTypeMemberDeclaration annotationTypeMemberDeclarationZeroOrMore
    |
    ;

annotationTypeMemberDeclaration
	:	annotationTypeElementDeclaration
	|	constantDeclaration
	|	classDeclaration
	|	interfaceDeclaration
	|	';'
	;

annotationTypeElementDeclaration
    :   annotationTypeElementModifierZeroOrMore unannType Identifier '(' ')' dimsZeroOrOne defaultValueZeroOrOne ';'
    ;

annotationTypeElementModifierZeroOrMore
    :   annotationTypeElementModifier annotationTypeElementModifierZeroOrMore
    |
    ;

defaultValueZeroOrOne
    :   defaultValue
    |
    ;

annotationTypeElementModifier
	:	annotation
	|	'public'
	|	'abstract'
	;

defaultValue
	:	'default' elementValue
	;

annotation
	:	normalAnnotation
	|	markerAnnotation
	|	singleElementAnnotation
	;

normalAnnotation
    :   '@' typeName '(' elementValuePairListZeroOrOne ')'
    ;

elementValuePairListZeroOrOne
    :   elementValuePairList
    |
    ;


elementValuePairList
    :   elementValuePair elementValuePairZeroOrMore
    ;

elementValuePairZeroOrMore
    :   ',' elementValuePair elementValuePairZeroOrMore
    |
    ;


elementValuePair
	:	Identifier '=' elementValue
	;

elementValue
	:	conditionalExpression
	|	elementValueArrayInitializer
	|	annotation
	;

elementValueArrayInitializer
    :   '{' elementValueListZeroOrOne ','? '}'
    ;

elementValueListZeroOrOne
    :   elementValueList
    |
    ;

elementValueList
    :   elementValue elementValueZeroOrMore
    ;

elementValueZeroOrMore
    :   ',' elementValue elementValueZeroOrMore
    |
    ;


markerAnnotation
	:	'@' typeName
	;

singleElementAnnotation
	:	'@' typeName '(' elementValue ')'
	;

/*
 * Productions from §10 (Arrays)
 */

arrayInitializer
	:	'{' variableInitializerList? ','? '}'
	;

variableInitializerList
    :   variableInitializer variableInitializerZeroOrMore
    ;

variableInitializerZeroOrMore
    :   ',' variableInitializer variableInitializerZeroOrMore
    |
    ;

/*
 * Productions from §14 (Blocks and Statements)
 */

block
    :   '{' blockStatementsZeroOrOne '}'
    ;

blockStatements
	:	blockStatement blockStatementsZeroOrMore
	;

blockStatementsZeroOrMore
	:	blockStatement blockStatementsZeroOrMore
	|
	;

blockStatement
	:	localVariableDeclarationStatement
	|	classDeclaration
	|	statement
	;

localVariableDeclarationStatement
	:	localVariableDeclaration ';'
	;

localVariableDeclaration
    :   variableModifierZeroOrMore unannType variableDeclaratorList
    ;



statement
	:	statementWithoutTrailingSubstatement
	|	labeledStatement
	|	ifThenStatement
	|	ifThenElseStatement
	|	whileStatement
	|	forStatement
	;

statementNoShortIf
	:	statementWithoutTrailingSubstatement
	|	labeledStatementNoShortIf
	|	ifThenElseStatementNoShortIf
	|	whileStatementNoShortIf
	|	forStatementNoShortIf
	;

statementWithoutTrailingSubstatement
	:	block
	|	emptyStatement_
	|	expressionStatement
	|	assertStatement
	|	switchStatement
	|	doStatement
	|	breakStatement
	|	continueStatement
	|	returnStatement
	|	synchronizedStatement
	|	throwStatement
	|	tryStatement
	;

emptyStatement_
	:	';'
	;

labeledStatement
	:	Identifier ':' statement
	;

labeledStatementNoShortIf
	:	Identifier ':' statementNoShortIf
	;

expressionStatement
	:	statementExpression ';'
	;

statementExpression
	:	assignment
	|	preIncrementExpression
	|	preDecrementExpression
	|	postIncrementExpression
	|	postDecrementExpression
	|	methodInvocation
	|	classInstanceCreationExpression
	;

ifThenStatement
	:	'if' '(' expression ')' statement
	;

ifThenElseStatement
	:	'if' '(' expression ')' statementNoShortIf 'else' statement
	;

ifThenElseStatementNoShortIf
	:	'if' '(' expression ')' statementNoShortIf 'else' statementNoShortIf
	;

assertStatement
	:	'assert' expression ';'
	|	'assert' expression ':' expression ';'
	;

switchStatement
	:	'switch' '(' expression ')' switchBlock
	;

switchBlock
    :   '{' switchBlockStatementGroupZeroOrMore switchLabelZeroOrMore '}'
    ;

switchBlockStatementGroupZeroOrMore
    :   switchBlockStatementGroup switchBlockStatementGroupZeroOrMore
    |
    ;
    
switchLabelZeroOrMore
    :   switchLabel switchLabelZeroOrMore
    |
    ;


switchBlockStatementGroup
	:	switchLabels blockStatements
	;

switchLabels
    :   switchLabel switchLabelZeroOrMore
    ;



switchLabel
	:	'case' constantExpression ':'
	|	'case' enumConstantName ':'
	|	'default' ':'
	;

enumConstantName
	:	Identifier
	;

whileStatement
	:	'while' '(' expression ')' statement
	;

whileStatementNoShortIf
	:	'while' '(' expression ')' statementNoShortIf
	;

doStatement
	:	'do' statement 'while' '(' expression ')' ';'
	;

forStatement
	:	basicForStatement
	|	enhancedForStatement
	;

forStatementNoShortIf
	:	basicForStatementNoShortIf
	|	enhancedForStatementNoShortIf
	;

basicForStatement
    :   'for' '(' forInitZeroOrOne ';' expressionZeroOrOne ';' forUpdateZeroOrOne ')' statement
    ;

forInitZeroOrOne
    :   forInit
    |
    ;

forUpdateZeroOrOne
    :   forUpdate
    |
    ;
expressionZeroOrOne
	:	expression
	|
	;

basicForStatementNoShortIf
    :   'for' '(' forInitZeroOrOne ';' expressionZeroOrOne ';' forUpdateZeroOrOne ')' statementNoShortIf
    ;


forInit
	:	statementExpressionList
	|	localVariableDeclaration
	;

forUpdate
	:	statementExpressionList
	;

statementExpressionList
    :   statementExpression statementExpressionZeroOrMore
    ;

statementExpressionZeroOrMore
    :   ',' statementExpression statementExpressionZeroOrMore
    |
    ;


enhancedForStatement
    :   'for' '(' variableModifierZeroOrMore unannType variableDeclaratorId ':' expression ')' statement
    ;


enhancedForStatementNoShortIf
    :   'for' '(' variableModifierZeroOrMore unannType variableDeclaratorId ':' expression ')' statementNoShortIf
    ;


breakStatement
    :   'break' Identifier? ';'
    ;



continueStatement
	:	'continue' Identifier? ';'
	;

returnStatement
	:	'return' expressionZeroOrOne ';'
	;

throwStatement
	:	'throw' expression ';'
	;

synchronizedStatement
	:	'synchronized' '(' expression ')' block
	;

tryStatement
	:	'try' block catches
	|	'try' block catchesZeroOrOne finally_
	|	tryWithResourcesStatement
	;

catchesZeroOrOne
	:	catches
	|
	;

catches
	:	catchClause catchClauseZeroOrMore
	;
	
catchClauseZeroOrMore
	:catchClause
	|
	;
	

catchClause
	:	'catch' '(' catchFormalParameter ')' block
	;

catchFormalParameter
	:	variableModifierZeroOrMore catchType variableDeclaratorId
	;


catchType
    :   unannClassType ('|' classTypeZeroOrMore)
    ;

classTypeZeroOrMore
    :   '|' classType classTypeZeroOrMore
    |
    ;


finally_
	:	'finally' block
	;

tryWithResourcesStatement
    :   'try' resourceSpecification block catchesZeroOrOne finallyZeroOrOne
    ;


finallyZeroOrOne
    :   finally_
    |
    ;

resourceSpecification
    :   '(' resourceList resourceOptionalSemicolonOneOrMore ')'
    ;

resourceOptionalSemicolon
    :   ';'
    |
    ;
   
resourceOptionalSemicolonOneOrMore
	:resourceOptionalSemicolon
	;

resourceList
    :   resource resourceZeroOrMore
    ;

resourceZeroOrMore
    :   ';' resource resourceZeroOrMore
    |
    ;

resource
    :   variableModifierZeroOrMore unannType variableDeclaratorId '=' expression
    ;



/*
 * Productions from §15 (Expressions)
 */

primary
    :   (primaryNoNewArray_lfno_primary | arrayCreationExpression) primaryNoNewArray_lf_primaryZeroOrMore
    ;

primaryNoNewArray_lf_primaryZeroOrMore
    :   primaryNoNewArray_lf_primary primaryNoNewArray_lf_primaryZeroOrMore
    |
    ;


primaryNoNewArray
    :   literal
    |   typeName arrayDimsNoNewArrayZeroOrMore '.' 'class'
    |   'void' '.' 'class'
    |   'this'
    |   typeName '.' 'this'
    |   '(' expression ')'
    |   classInstanceCreationExpression
    |   fieldAccess
    |   arrayAccess
    |   methodInvocation
    |   methodReference
    ;

arrayDimsNoNewArray
    :   '[' ']'
    ;
    
arrayDimsNoNewArrayZeroOrMore
	:arrayDimsNoNewArray
	|
	;


primaryNoNewArray_lf_arrayAccess
	:
	;

primaryNoNewArray_lfno_arrayAccess
	:	literal
	|	typeName arrayDimsNoNewArrayZeroOrMore '.' 'class'
	|	'void' '.' 'class'
	|	'this'
	|	typeName '.' 'this'
	|	'(' expression ')'
	|	classInstanceCreationExpression
	|	fieldAccess
	|	methodInvocation
	|	methodReference
	;

primaryNoNewArray_lf_primary
	:	classInstanceCreationExpression_lf_primary
	|	fieldAccess_lf_primary
	|	arrayAccess_lf_primary
	|	methodInvocation_lf_primary
	|	methodReference_lf_primary
	;

primaryNoNewArray_lf_primary_lf_arrayAccess_lf_primary
	:
	;

primaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primary
	:	classInstanceCreationExpression_lf_primary
	|	fieldAccess_lf_primary
	|	methodInvocation_lf_primary
	|	methodReference_lf_primary
	;

primaryNoNewArray_lfno_primary
	:	literal
	|	typeName arrayDimsNoNewArrayZeroOrMore '.' 'class'
	|	unannPrimitiveType arrayDimsNoNewArrayZeroOrMore '.' 'class'
	|	'void' '.' 'class'
	|	'this'
	|	typeName '.' 'this'
	|	'(' expression ')'
	|	classInstanceCreationExpression_lfno_primary
	|	fieldAccess_lfno_primary
	|	arrayAccess_lfno_primary
	|	methodInvocation_lfno_primary
	|	methodReference_lfno_primary
	;

primaryNoNewArray_lfno_primary_lf_arrayAccess_lfno_primary
	:
	;

primaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primary
	:	literal
	|	typeName arrayDimsNoNewArrayZeroOrMore '.' 'class'
	|	unannPrimitiveType arrayDimsNoNewArrayZeroOrMore '.' 'class'
	|	'void' '.' 'class'
	|	'this'
	|	typeName '.' 'this'
	|	'(' expression ')'
	|	classInstanceCreationExpression_lfno_primary
	|	fieldAccess_lfno_primary
	|	methodInvocation_lfno_primary
	|	methodReference_lfno_primary
	;

classInstanceCreationExpression
    :   'new' typeArgumentsZeroOrOne annotationZeroOrMore Identifier classInstanceCreationRest
    |   expressionName '.' 'new' typeArgumentsZeroOrOne annotationZeroOrMore Identifier classInstanceCreationRest
    |   primary '.' 'new' typeArgumentsZeroOrOne annotationZeroOrMore Identifier classInstanceCreationRest
    ;

classInstanceCreationRest
    :   '.' annotationZeroOrMore Identifier typeArgumentsOrDiamondZeroOrOne '(' argumentListZeroOrOne ')' classBodyZeroOrOne
    ;

typeArgumentsOrDiamondZeroOrOne
    :   typeArgumentsOrDiamond
    |
    ;
typeArgumentsOrDiamond
    :   typeArguments
    |   '<' '>'
    ;

classInstanceCreationExpression_lf_primary
    :   '.' 'new' typeArgumentsZeroOrOne annotationZeroOrMore Identifier typeArgumentsOrDiamondZeroOrOne '(' argumentListZeroOrOne ')' classBodyZeroOrOne
    ;

classInstanceCreationExpression_lfno_primary
    :   'new' typeArgumentsZeroOrOne annotationZeroOrMore Identifier classInstanceCreationRest_lfno_primary
    |   expressionName '.' 'new' typeArgumentsZeroOrOne annotationZeroOrMore Identifier classInstanceCreationRest_lfno_primary
    ;
classInstanceCreationRest_lfno_primary
    :   ('.' annotationZeroOrMore Identifier typeArgumentsOrDiamondZeroOrOne '(' argumentListZeroOrOne ')' classBodyZeroOrOne)
    ;

fieldAccess
	:	primary '.' Identifier
	|	'super' '.' Identifier
	|	typeName '.' 'super' '.' Identifier
	;

fieldAccess_lf_primary
	:	'.' Identifier
	;

fieldAccess_lfno_primary
	:	'super' '.' Identifier
	|	typeName '.' 'super' '.' Identifier
	;

arrayAccess
    :   arrayAccessPrimary arrayAccessRestZeroOrMore
    ;

arrayAccessPrimary
    :   expressionName '[' expression ']'
    |   primaryNoNewArray_lfno_arrayAccess '[' expression ']'
    ;

arrayAccessRest
    :   primaryNoNewArray_lf_arrayAccess '[' expression ']'
    ;

arrayAccessRestZeroOrMore
	:arrayAccessRest
	|
	;

arrayAccess_lf_primary
	:	primaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primary '[' expression ']'
		primaryNoNewArray_lfZeroOrMore
	;
	
primaryNoNewArray_lfZeroOrMore
	:(	primaryNoNewArray_lf_primary_lf_arrayAccess_lf_primary '[' expression ']')
	|
	;
	

arrayAccess_lfno_primary
	:	(	expressionName '[' expression ']'
		|	primaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primary '[' expression ']'
		)
		primaryNoNewArray_lfZeroOrMore
	;

methodInvocation
	:	methodName '(' argumentListZeroOrOne ')'
	|	typeName '.' typeArgumentsZeroOrOne Identifier '(' argumentListZeroOrOne ')'
	|	expressionName '.' typeArgumentsZeroOrOne Identifier '(' argumentListZeroOrOne ')'
	|	primary '.' typeArgumentsZeroOrOne Identifier '(' argumentListZeroOrOne ')'
	|	'super' '.' typeArgumentsZeroOrOne Identifier '(' argumentListZeroOrOne ')'
	|	typeName '.' 'super' '.' typeArgumentsZeroOrOne Identifier '(' argumentListZeroOrOne ')'
	;

methodInvocation_lf_primary
	:	'.' typeArgumentsZeroOrOne Identifier '(' argumentListZeroOrOne ')'
	;

methodInvocation_lfno_primary
	:	methodName '(' argumentListZeroOrOne ')'
	|	typeName '.' typeArgumentsZeroOrOne Identifier '(' argumentListZeroOrOne ')'
	|	expressionName '.' typeArgumentsZeroOrOne Identifier '(' argumentListZeroOrOne ')'
	|	'super' '.' typeArgumentsZeroOrOne Identifier '(' argumentListZeroOrOne ')'
	|	typeName '.' 'super' '.' typeArgumentsZeroOrOne Identifier '(' argumentListZeroOrOne ')'
	;

argumentList
	:	expression commaExoZeroOrMore
	;

commaExoZeroOrMore
	:(',' expression)
	|
	;

methodReference
	:	expressionName '::' typeArgumentsZeroOrOne Identifier
	|	referenceType '::' typeArgumentsZeroOrOne Identifier
	|	primary '::' typeArgumentsZeroOrOne Identifier
	|	'super' '::' typeArgumentsZeroOrOne Identifier
	|	typeName '.' 'super' '::' typeArgumentsZeroOrOne Identifier
	|	classType '::' typeArgumentsZeroOrOne 'new'
	|	arrayType '::' 'new'
	;

methodReference_lf_primary
	:	'::' typeArgumentsZeroOrOne Identifier
	;

methodReference_lfno_primary
	:	expressionName '::' typeArgumentsZeroOrOne Identifier
	|	referenceType '::' typeArgumentsZeroOrOne Identifier
	|	'super' '::' typeArgumentsZeroOrOne Identifier
	|	typeName '.' 'super' '::' typeArgumentsZeroOrOne Identifier
	|	classType '::' typeArgumentsZeroOrOne 'new'
	|	arrayType '::' 'new'
	;

arrayCreationExpression
	:	'new' primitiveType dimExprs dimsZeroOrOne
	|	'new' classOrInterfaceType dimExprs dimsZeroOrOne
	|	'new' primitiveType dims arrayInitializer
	|	'new' classOrInterfaceType dims arrayInitializer
	;

dimExprs
	:	dimExpr dimExprZeroOrMore
	;

dimExprZeroOrMore
	:dimExpr
	|
	;
	
dimExpr
	:	annotationZeroOrMore '[' expression ']'
	;

constantExpression
	:	expression
	;

expression
	:	lambdaExpression
	|	assignmentExpression
	;

lambdaExpression
	:	lambdaParameters '->' lambdaBody
	;

lambdaParameters
	:	Identifier
	|	'(' formalParameterListZeroOrOne ')'
	|	'(' inferredFormalParameterList ')'
	;

inferredFormalParameterList
    :   Identifier inferredFormalParameterCommaSeparated
    ;

inferredFormalParameterCommaSeparated
    :   ',' Identifier inferredFormalParameterCommaSeparated
    |
    ;


lambdaBody
	:	expression
	|	block
	;

assignmentExpression
	:	conditionalExpression
	|	assignment
	;

assignment
	:	leftHandSide assignmentOperator expression
	;

leftHandSide
	:	expressionName
	|	fieldAccess
	|	arrayAccess
	;

assignmentOperator
	:	'='
	|	'*='
	|	'/='
	|	'%='
	|	'+='
	|	'-='
	|	'<<='
	|	'>>='
	|	'>>>='
	|	'&='
	|	'^='
	|	'|='
	;

conditionalExpression
	:	conditionalOrExpression
	|	conditionalOrExpression '?' expression ':' conditionalExpression
	;

conditionalOrExpression
	:	conditionalAndExpression
	|	conditionalOrExpression '||' conditionalAndExpression
	;

conditionalAndExpression
	:	inclusiveOrExpression
	|	conditionalAndExpression '&&' inclusiveOrExpression
	;

inclusiveOrExpression
	:	exclusiveOrExpression
	|	inclusiveOrExpression '|' exclusiveOrExpression
	;

exclusiveOrExpression
	:	andExpression
	|	exclusiveOrExpression '^' andExpression
	;

andExpression
	:	equalityExpression
	|	andExpression '&' equalityExpression
	;

equalityExpression
	:	relationalExpression
	|	equalityExpression '==' relationalExpression
	|	equalityExpression '!=' relationalExpression
	;

relationalExpression
	:	shiftExpression
	|	relationalExpression '<' shiftExpression
	|	relationalExpression '>' shiftExpression
	|	relationalExpression '<=' shiftExpression
	|	relationalExpression '>=' shiftExpression
	|	relationalExpression 'instanceof' referenceType
	;

shiftExpression
	:	additiveExpression
	|	shiftExpression '<' '<' additiveExpression
	|	shiftExpression '>' '>' additiveExpression
	|	shiftExpression '>' '>' '>' additiveExpression
	;

additiveExpression
	:	multiplicativeExpression
	|	additiveExpression '+' multiplicativeExpression
	|	additiveExpression '-' multiplicativeExpression
	;

multiplicativeExpression
	:	unaryExpression
	|	multiplicativeExpression '*' unaryExpression
	|	multiplicativeExpression '/' unaryExpression
	|	multiplicativeExpression '%' unaryExpression
	;

unaryExpression
	:	preIncrementExpression
	|	preDecrementExpression
	|	'+' unaryExpression
	|	'-' unaryExpression
	|	unaryExpressionNotPlusMinus
	;

preIncrementExpression
	:	'++' unaryExpression
	;

preDecrementExpression
	:	'--' unaryExpression
	;

unaryExpressionNotPlusMinus
	:	postfixExpression
	|	'~' unaryExpression
	|	'!' unaryExpression
	|	castExpression
	;

postfixExpression
	:	(	primary
		|	expressionName
		)
		postInfixExpZeroOrMore
	;
	
postInfixExpZeroOrMore
	:(	postIncrementExpression_lf_postfixExpression|postDecrementExpression_lf_postfixExpression)
	|
	;

postIncrementExpression
	:	postfixExpression '++'
	;

postIncrementExpression_lf_postfixExpression
	:	'++'
	;

postDecrementExpression
	:	postfixExpression '--'
	;

postDecrementExpression_lf_postfixExpression
	:	'--'
	;

castExpression
	:	'(' primitiveType ')' unaryExpression
	|	'(' referenceType additionalBoundZeroOrMore ')' unaryExpressionNotPlusMinus
	|	'(' referenceType additionalBoundZeroOrMore ')' lambdaExpression
	;
