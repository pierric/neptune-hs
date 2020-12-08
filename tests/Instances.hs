{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import NeptuneBackend.Model
import NeptuneBackend.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)
    
arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models
 
instance Arbitrary AchievementsDTO where
  arbitrary = sized genAchievementsDTO

genAchievementsDTO :: Int -> Gen AchievementsDTO
genAchievementsDTO n =
  AchievementsDTO
    <$> arbitraryReduced n -- achievementsDTOEarned :: [AchievementTypeDTO]
  
instance Arbitrary AuthorizedUserDTO where
  arbitrary = sized genAuthorizedUserDTO

genAuthorizedUserDTO :: Int -> Gen AuthorizedUserDTO
genAuthorizedUserDTO n =
  AuthorizedUserDTO
    <$> arbitrary -- authorizedUserDTOUsername :: Text
  
instance Arbitrary BatchChannelValueErrorDTO where
  arbitrary = sized genBatchChannelValueErrorDTO

genBatchChannelValueErrorDTO :: Int -> Gen BatchChannelValueErrorDTO
genBatchChannelValueErrorDTO n =
  BatchChannelValueErrorDTO
    <$> arbitrary -- batchChannelValueErrorDTOChannelId :: Text
    <*> arbitrary -- batchChannelValueErrorDTOX :: Double
    <*> arbitraryReduced n -- batchChannelValueErrorDTOError :: Error
  
instance Arbitrary BatchExperimentUpdateResult where
  arbitrary = sized genBatchExperimentUpdateResult

genBatchExperimentUpdateResult :: Int -> Gen BatchExperimentUpdateResult
genBatchExperimentUpdateResult n =
  BatchExperimentUpdateResult
    <$> arbitrary -- batchExperimentUpdateResultExperimentId :: Text
    <*> arbitraryReducedMaybe n -- batchExperimentUpdateResultError :: Maybe Error
  
instance Arbitrary Channel where
  arbitrary = sized genChannel

genChannel :: Int -> Gen Channel
genChannel n =
  Channel
    <$> arbitrary -- channelId :: Text
    <*> arbitrary -- channelName :: Text
    <*> arbitraryReduced n -- channelChannelType :: ChannelType
    <*> arbitraryReducedMaybe n -- channelLastX :: Maybe Double
  
instance Arbitrary ChannelDTO where
  arbitrary = sized genChannelDTO

genChannelDTO :: Int -> Gen ChannelDTO
genChannelDTO n =
  ChannelDTO
    <$> arbitrary -- channelDTOId :: Text
    <*> arbitrary -- channelDTOName :: Text
    <*> arbitraryReduced n -- channelDTOChannelType :: ChannelTypeEnum
    <*> arbitraryReducedMaybe n -- channelDTOLastX :: Maybe Double
  
instance Arbitrary ChannelParams where
  arbitrary = sized genChannelParams

genChannelParams :: Int -> Gen ChannelParams
genChannelParams n =
  ChannelParams
    <$> arbitrary -- channelParamsName :: Text
    <*> arbitraryReduced n -- channelParamsChannelType :: ChannelTypeEnum
  
instance Arbitrary ChannelWithValue where
  arbitrary = sized genChannelWithValue

genChannelWithValue :: Int -> Gen ChannelWithValue
genChannelWithValue n =
  ChannelWithValue
    <$> arbitrary -- channelWithValueX :: Double
    <*> arbitrary -- channelWithValueY :: Text
    <*> arbitraryReduced n -- channelWithValueChannelType :: ChannelType
    <*> arbitrary -- channelWithValueChannelName :: Text
    <*> arbitrary -- channelWithValueChannelId :: Text
  
instance Arbitrary ChannelWithValueDTO where
  arbitrary = sized genChannelWithValueDTO

genChannelWithValueDTO :: Int -> Gen ChannelWithValueDTO
genChannelWithValueDTO n =
  ChannelWithValueDTO
    <$> arbitrary -- channelWithValueDTOX :: Double
    <*> arbitrary -- channelWithValueDTOY :: Text
    <*> arbitraryReduced n -- channelWithValueDTOChannelType :: ChannelTypeEnum
    <*> arbitrary -- channelWithValueDTOChannelName :: Text
    <*> arbitrary -- channelWithValueDTOChannelId :: Text
  
instance Arbitrary Chart where
  arbitrary = sized genChart

genChart :: Int -> Gen Chart
genChart n =
  Chart
    <$> arbitrary -- chartId :: Text
    <*> arbitrary -- chartName :: Text
    <*> arbitraryReduced n -- chartSeries :: [Series]
  
instance Arbitrary ChartDefinition where
  arbitrary = sized genChartDefinition

genChartDefinition :: Int -> Gen ChartDefinition
genChartDefinition n =
  ChartDefinition
    <$> arbitrary -- chartDefinitionName :: Text
    <*> arbitraryReduced n -- chartDefinitionSeries :: [SeriesDefinition]
  
instance Arbitrary ChartSet where
  arbitrary = sized genChartSet

genChartSet :: Int -> Gen ChartSet
genChartSet n =
  ChartSet
    <$> arbitraryReducedMaybe n -- chartSetIsEditable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- chartSetDefaultChartsEnabled :: Maybe Bool
    <*> arbitrary -- chartSetProjectId :: Text
    <*> arbitrary -- chartSetId :: Text
    <*> arbitrary -- chartSetName :: Text
    <*> arbitraryReducedMaybe n -- chartSetCharts :: Maybe [Chart]
  
instance Arbitrary ChartSetParams where
  arbitrary = sized genChartSetParams

genChartSetParams :: Int -> Gen ChartSetParams
genChartSetParams n =
  ChartSetParams
    <$> arbitrary -- chartSetParamsName :: Text
    <*> arbitraryReducedMaybe n -- chartSetParamsCharts :: Maybe [ChartDefinition]
    <*> arbitraryReducedMaybe n -- chartSetParamsDefaultChartsEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- chartSetParamsIsEditable :: Maybe Bool
  
instance Arbitrary Charts where
  arbitrary = sized genCharts

genCharts :: Int -> Gen Charts
genCharts n =
  Charts
    <$> arbitraryReduced n -- chartsManualCharts :: [Chart]
    <*> arbitraryReduced n -- chartsDefaultCharts :: [Chart]
  
instance Arbitrary ClientConfig where
  arbitrary = sized genClientConfig

genClientConfig :: Int -> Gen ClientConfig
genClientConfig n =
  ClientConfig
    <$> arbitrary -- clientConfigApiUrl :: Text
    <*> arbitrary -- clientConfigApplicationUrl :: Text
    <*> arbitraryReduced n -- clientConfigPyLibVersions :: ClientVersionsConfigDTO
  
instance Arbitrary ClientVersionsConfigDTO where
  arbitrary = sized genClientVersionsConfigDTO

genClientVersionsConfigDTO :: Int -> Gen ClientVersionsConfigDTO
genClientVersionsConfigDTO n =
  ClientVersionsConfigDTO
    <$> arbitraryReducedMaybe n -- clientVersionsConfigDTOMinRecommendedVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- clientVersionsConfigDTOMinCompatibleVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- clientVersionsConfigDTOMaxCompatibleVersion :: Maybe Text
  
instance Arbitrary CompletedExperimentParams where
  arbitrary = sized genCompletedExperimentParams

genCompletedExperimentParams :: Int -> Gen CompletedExperimentParams
genCompletedExperimentParams n =
  CompletedExperimentParams
    <$> arbitraryReduced n -- completedExperimentParamsState :: ExperimentState
    <*> arbitrary -- completedExperimentParamsTraceback :: Text
  
instance Arbitrary ComponentStatus where
  arbitrary = sized genComponentStatus

genComponentStatus :: Int -> Gen ComponentStatus
genComponentStatus n =
  ComponentStatus
    <$> arbitrary -- componentStatusName :: Text
    <*> arbitrary -- componentStatusStatus :: Text
  
instance Arbitrary ComponentVersion where
  arbitrary = sized genComponentVersion

genComponentVersion :: Int -> Gen ComponentVersion
genComponentVersion n =
  ComponentVersion
    <$> arbitraryReduced n -- componentVersionName :: NameEnum
    <*> arbitrary -- componentVersionVersion :: Text
  
instance Arbitrary ConfigInfo where
  arbitrary = sized genConfigInfo

genConfigInfo :: Int -> Gen ConfigInfo
genConfigInfo n =
  ConfigInfo
    <$> arbitrary -- configInfoMaxFormContentSize :: Int
  
instance Arbitrary CreateSessionParamsDTO where
  arbitrary = sized genCreateSessionParamsDTO

genCreateSessionParamsDTO :: Int -> Gen CreateSessionParamsDTO
genCreateSessionParamsDTO n =
  CreateSessionParamsDTO
    <$> arbitrary -- createSessionParamsDTOSuccessUrl :: Text
    <*> arbitrary -- createSessionParamsDTOFailureUrl :: Text
  
instance Arbitrary CustomerDTO where
  arbitrary = sized genCustomerDTO

genCustomerDTO :: Int -> Gen CustomerDTO
genCustomerDTO n =
  CustomerDTO
    <$> arbitraryReducedMaybe n -- customerDTONumberOfUsers :: Maybe Integer
    <*> arbitrary -- customerDTOUserPriceInCents :: Integer
    <*> arbitraryReduced n -- customerDTOPricingPlan :: PricingPlanDTO
    <*> arbitraryReducedMaybe n -- customerDTODiscount :: Maybe DiscountDTO
  
instance Arbitrary DiscountCodeDTO where
  arbitrary = sized genDiscountCodeDTO

genDiscountCodeDTO :: Int -> Gen DiscountCodeDTO
genDiscountCodeDTO n =
  DiscountCodeDTO
    <$> arbitrary -- discountCodeDTOCode :: Text
  
instance Arbitrary DiscountDTO where
  arbitrary = sized genDiscountDTO

genDiscountDTO :: Int -> Gen DiscountDTO
genDiscountDTO n =
  DiscountDTO
    <$> arbitraryReducedMaybe n -- discountDTOAmountOffPercentage :: Maybe Integer
    <*> arbitraryReducedMaybe n -- discountDTOAmountOffInCents :: Maybe Integer
    <*> arbitraryReducedMaybe n -- discountDTOEnd :: Maybe DateTime
  
instance Arbitrary DownloadPrepareRequestDTO where
  arbitrary = sized genDownloadPrepareRequestDTO

genDownloadPrepareRequestDTO :: Int -> Gen DownloadPrepareRequestDTO
genDownloadPrepareRequestDTO n =
  DownloadPrepareRequestDTO
    <$> arbitrary -- downloadPrepareRequestDTOId :: Text
    <*> arbitraryReducedMaybe n -- downloadPrepareRequestDTODownloadUrl :: Maybe Text
  
instance Arbitrary EditExperimentParams where
  arbitrary = sized genEditExperimentParams

genEditExperimentParams :: Int -> Gen EditExperimentParams
genEditExperimentParams n =
  EditExperimentParams
    <$> arbitraryReducedMaybe n -- editExperimentParamsName :: Maybe Text
    <*> arbitraryReducedMaybe n -- editExperimentParamsDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- editExperimentParamsTags :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- editExperimentParamsProperties :: Maybe [KeyValueProperty]
  
instance Arbitrary Error where
  arbitrary = sized genError

genError :: Int -> Gen Error
genError n =
  Error
    <$> arbitrary -- errorCode :: Int
    <*> arbitrary -- errorMessage :: Text
    <*> arbitraryReducedMaybe n -- errorType :: Maybe ApiErrorTypeDTO
  
instance Arbitrary Experiment where
  arbitrary = sized genExperiment

genExperiment :: Int -> Gen Experiment
genExperiment n =
  Experiment
    <$> arbitraryReduced n -- experimentChannels :: [Channel]
    <*> arbitraryReduced n -- experimentState :: ExperimentState
    <*> arbitraryReducedMaybe n -- experimentTimeOfCompletion :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- experimentCheckpointId :: Maybe Text
    <*> arbitraryReduced n -- experimentPaths :: ExperimentPaths
    <*> arbitrary -- experimentResponding :: Bool
    <*> arbitrary -- experimentOrganizationId :: Text
    <*> arbitraryReduced n -- experimentStateTransitions :: StateTransitions
    <*> arbitraryReduced n -- experimentParameters :: [Parameter]
    <*> arbitraryReduced n -- experimentChannelsLastValues :: [ChannelWithValue]
    <*> arbitrary -- experimentStorageSize :: Integer
    <*> arbitrary -- experimentName :: Text
    <*> arbitraryReducedMaybe n -- experimentNotebookId :: Maybe Text
    <*> arbitrary -- experimentProjectName :: Text
    <*> arbitraryReducedMaybe n -- experimentHostname :: Maybe Text
    <*> arbitrary -- experimentTrashed :: Bool
    <*> arbitrary -- experimentDescription :: Text
    <*> arbitrary -- experimentTags :: [Text]
    <*> arbitrary -- experimentChannelsSize :: Integer
    <*> arbitraryReduced n -- experimentTimeOfCreation :: DateTime
    <*> arbitrary -- experimentProjectId :: Text
    <*> arbitrary -- experimentOrganizationName :: Text
    <*> arbitraryReducedMaybe n -- experimentIsCodeAccessible :: Maybe Bool
    <*> arbitraryReducedMaybe n -- experimentTraceback :: Maybe Text
    <*> arbitraryReducedMaybe n -- experimentEntrypoint :: Maybe Text
    <*> arbitrary -- experimentRunningTime :: Integer
    <*> arbitrary -- experimentId :: Text
    <*> arbitraryReduced n -- experimentInputs :: [InputMetadata]
    <*> arbitraryReduced n -- experimentProperties :: [KeyValueProperty]
    <*> arbitrary -- experimentShortId :: Text
    <*> arbitraryReduced n -- experimentComponentsVersions :: [ComponentVersion]
    <*> arbitrary -- experimentOwner :: Text
  
instance Arbitrary ExperimentCreationParams where
  arbitrary = sized genExperimentCreationParams

genExperimentCreationParams :: Int -> Gen ExperimentCreationParams
genExperimentCreationParams n =
  ExperimentCreationParams
    <$> arbitraryReducedMaybe n -- experimentCreationParamsMonitored :: Maybe Bool
    <*> arbitraryReducedMaybe n -- experimentCreationParamsHostname :: Maybe Text
    <*> arbitraryReducedMaybe n -- experimentCreationParamsCheckpointId :: Maybe Text
    <*> arbitrary -- experimentCreationParamsProjectId :: Text
    <*> arbitraryReducedMaybe n -- experimentCreationParamsGitInfo :: Maybe GitInfoDTO
    <*> arbitraryReduced n -- experimentCreationParamsProperties :: [KeyValueProperty]
    <*> arbitraryReducedMaybe n -- experimentCreationParamsConfigPath :: Maybe Text
    <*> arbitrary -- experimentCreationParamsExecArgsTemplate :: Text
    <*> arbitraryReduced n -- experimentCreationParamsParameters :: [Parameter]
    <*> arbitrary -- experimentCreationParamsEnqueueCommand :: Text
    <*> arbitrary -- experimentCreationParamsName :: Text
    <*> arbitraryReducedMaybe n -- experimentCreationParamsNotebookId :: Maybe Text
    <*> arbitraryReducedMaybe n -- experimentCreationParamsDescription :: Maybe Text
    <*> arbitrary -- experimentCreationParamsTags :: [Text]
    <*> arbitraryReducedMaybe n -- experimentCreationParamsAbortable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- experimentCreationParamsEntrypoint :: Maybe Text
  
instance Arbitrary ExperimentPaths where
  arbitrary = sized genExperimentPaths

genExperimentPaths :: Int -> Gen ExperimentPaths
genExperimentPaths n =
  ExperimentPaths
    <$> arbitrary -- experimentPathsOutput :: Text
    <*> arbitrary -- experimentPathsSource :: Text
  
instance Arbitrary ExperimentsAttributesNamesDTO where
  arbitrary = sized genExperimentsAttributesNamesDTO

genExperimentsAttributesNamesDTO :: Int -> Gen ExperimentsAttributesNamesDTO
genExperimentsAttributesNamesDTO n =
  ExperimentsAttributesNamesDTO
    <$> arbitrary -- experimentsAttributesNamesDTOTextParametersNames :: [Text]
    <*> arbitrary -- experimentsAttributesNamesDTOPropertiesNames :: [Text]
    <*> arbitrary -- experimentsAttributesNamesDTONumericChannelsNames :: [Text]
    <*> arbitrary -- experimentsAttributesNamesDTONumericParametersNames :: [Text]
    <*> arbitrary -- experimentsAttributesNamesDTOTextChannelsNames :: [Text]
  
instance Arbitrary File where
  arbitrary = sized genFile

genFile :: Int -> Gen File
genFile n =
  File
    <$> arbitrary -- filePath :: Text
  
instance Arbitrary GitCommitDTO where
  arbitrary = sized genGitCommitDTO

genGitCommitDTO :: Int -> Gen GitCommitDTO
genGitCommitDTO n =
  GitCommitDTO
    <$> arbitrary -- gitCommitDTOAuthorEmail :: Text
    <*> arbitrary -- gitCommitDTOCommitId :: Text
    <*> arbitrary -- gitCommitDTOMessage :: Text
    <*> arbitraryReduced n -- gitCommitDTOCommitDate :: DateTime
    <*> arbitrary -- gitCommitDTOAuthorName :: Text
  
instance Arbitrary GitInfoDTO where
  arbitrary = sized genGitInfoDTO

genGitInfoDTO :: Int -> Gen GitInfoDTO
genGitInfoDTO n =
  GitInfoDTO
    <$> arbitraryReducedMaybe n -- gitInfoDTOCurrentBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- gitInfoDTORemotes :: Maybe [Text]
    <*> arbitraryReduced n -- gitInfoDTOCommit :: GitCommitDTO
    <*> arbitrary -- gitInfoDTORepositoryDirty :: Bool
  
instance Arbitrary GlobalConfiguration where
  arbitrary = sized genGlobalConfiguration

genGlobalConfiguration :: Int -> Gen GlobalConfiguration
genGlobalConfiguration n =
  GlobalConfiguration
    <$> arbitraryReduced n -- globalConfigurationLicenseExpiration :: DateTime
  
instance Arbitrary InputChannelValues where
  arbitrary = sized genInputChannelValues

genInputChannelValues :: Int -> Gen InputChannelValues
genInputChannelValues n =
  InputChannelValues
    <$> arbitrary -- inputChannelValuesChannelId :: Text
    <*> arbitraryReduced n -- inputChannelValuesValues :: [Point]
  
instance Arbitrary InputImageDTO where
  arbitrary = sized genInputImageDTO

genInputImageDTO :: Int -> Gen InputImageDTO
genInputImageDTO n =
  InputImageDTO
    <$> arbitraryReducedMaybe n -- inputImageDTOName :: Maybe Text
    <*> arbitraryReducedMaybe n -- inputImageDTODescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- inputImageDTOData :: Maybe Text
  
instance Arbitrary InputMetadata where
  arbitrary = sized genInputMetadata

genInputMetadata :: Int -> Gen InputMetadata
genInputMetadata n =
  InputMetadata
    <$> arbitrary -- inputMetadataSource :: Text
    <*> arbitrary -- inputMetadataDestination :: Text
    <*> arbitrary -- inputMetadataSize :: Integer
  
instance Arbitrary KeyValueProperty where
  arbitrary = sized genKeyValueProperty

genKeyValueProperty :: Int -> Gen KeyValueProperty
genKeyValueProperty n =
  KeyValueProperty
    <$> arbitrary -- keyValuePropertyKey :: Text
    <*> arbitrary -- keyValuePropertyValue :: Text
  
instance Arbitrary LimitedChannelValuesDTO where
  arbitrary = sized genLimitedChannelValuesDTO

genLimitedChannelValuesDTO :: Int -> Gen LimitedChannelValuesDTO
genLimitedChannelValuesDTO n =
  LimitedChannelValuesDTO
    <$> arbitrary -- limitedChannelValuesDTOChannelId :: Text
    <*> arbitraryReduced n -- limitedChannelValuesDTOValues :: [PointValueDTO]
    <*> arbitrary -- limitedChannelValuesDTOTotalItemCount :: Int
  
instance Arbitrary Link where
  arbitrary = sized genLink

genLink :: Int -> Gen Link
genLink n =
  Link
    <$> arbitrary -- linkUrl :: Text
  
instance Arbitrary LinkDTO where
  arbitrary = sized genLinkDTO

genLinkDTO :: Int -> Gen LinkDTO
genLinkDTO n =
  LinkDTO
    <$> arbitrary -- linkDTOUrl :: Text
  
instance Arbitrary LoginActionsListDTO where
  arbitrary = sized genLoginActionsListDTO

genLoginActionsListDTO :: Int -> Gen LoginActionsListDTO
genLoginActionsListDTO n =
  LoginActionsListDTO
    <$> arbitraryReduced n -- loginActionsListDTORequiredActions :: [LoginActionDTO]
  
instance Arbitrary NeptuneApiToken where
  arbitrary = sized genNeptuneApiToken

genNeptuneApiToken :: Int -> Gen NeptuneApiToken
genNeptuneApiToken n =
  NeptuneApiToken
    <$> arbitrary -- neptuneApiTokenToken :: Text
  
instance Arbitrary NeptuneOauthToken where
  arbitrary = sized genNeptuneOauthToken

genNeptuneOauthToken :: Int -> Gen NeptuneOauthToken
genNeptuneOauthToken n =
  NeptuneOauthToken
    <$> arbitrary -- neptuneOauthTokenAccessToken :: Text
    <*> arbitrary -- neptuneOauthTokenRefreshToken :: Text
    <*> arbitrary -- neptuneOauthTokenUsername :: Text
  
instance Arbitrary NewOrganizationInvitationDTO where
  arbitrary = sized genNewOrganizationInvitationDTO

genNewOrganizationInvitationDTO :: Int -> Gen NewOrganizationInvitationDTO
genNewOrganizationInvitationDTO n =
  NewOrganizationInvitationDTO
    <$> arbitraryReduced n -- newOrganizationInvitationDTORoleGrant :: OrganizationRoleDTO
    <*> arbitrary -- newOrganizationInvitationDTOAddToAllProjects :: Bool
    <*> arbitrary -- newOrganizationInvitationDTOOrganizationIdentifier :: Text
    <*> arbitrary -- newOrganizationInvitationDTOInvitee :: Text
    <*> arbitraryReduced n -- newOrganizationInvitationDTOInvitationType :: InvitationTypeEnumDTO
  
instance Arbitrary NewOrganizationMemberDTO where
  arbitrary = sized genNewOrganizationMemberDTO

genNewOrganizationMemberDTO :: Int -> Gen NewOrganizationMemberDTO
genNewOrganizationMemberDTO n =
  NewOrganizationMemberDTO
    <$> arbitrary -- newOrganizationMemberDTOUserId :: Text
    <*> arbitraryReduced n -- newOrganizationMemberDTORole :: OrganizationRoleDTO
  
instance Arbitrary NewProjectDTO where
  arbitrary = sized genNewProjectDTO

genNewProjectDTO :: Int -> Gen NewProjectDTO
genNewProjectDTO n =
  NewProjectDTO
    <$> arbitrary -- newProjectDTOName :: Text
    <*> arbitraryReducedMaybe n -- newProjectDTODescription :: Maybe Text
    <*> arbitrary -- newProjectDTOProjectKey :: Text
    <*> arbitrary -- newProjectDTOOrganizationId :: Text
    <*> arbitraryReducedMaybe n -- newProjectDTOVisibility :: Maybe ProjectVisibilityDTO
    <*> arbitraryReducedMaybe n -- newProjectDTODisplayClass :: Maybe Text
  
instance Arbitrary NewProjectInvitationDTO where
  arbitrary = sized genNewProjectInvitationDTO

genNewProjectInvitationDTO :: Int -> Gen NewProjectInvitationDTO
genNewProjectInvitationDTO n =
  NewProjectInvitationDTO
    <$> arbitrary -- newProjectInvitationDTOProjectIdentifier :: Text
    <*> arbitrary -- newProjectInvitationDTOInvitee :: Text
    <*> arbitraryReduced n -- newProjectInvitationDTOInvitationType :: InvitationTypeEnumDTO
    <*> arbitraryReduced n -- newProjectInvitationDTORoleGrant :: ProjectRoleDTO
  
instance Arbitrary NewProjectMemberDTO where
  arbitrary = sized genNewProjectMemberDTO

genNewProjectMemberDTO :: Int -> Gen NewProjectMemberDTO
genNewProjectMemberDTO n =
  NewProjectMemberDTO
    <$> arbitrary -- newProjectMemberDTOUserId :: Text
    <*> arbitraryReduced n -- newProjectMemberDTORole :: ProjectRoleDTO
  
instance Arbitrary NewReservationDTO where
  arbitrary = sized genNewReservationDTO

genNewReservationDTO :: Int -> Gen NewReservationDTO
genNewReservationDTO n =
  NewReservationDTO
    <$> arbitrary -- newReservationDTOName :: Text
  
instance Arbitrary OrganizationCreationParamsDTO where
  arbitrary = sized genOrganizationCreationParamsDTO

genOrganizationCreationParamsDTO :: Int -> Gen OrganizationCreationParamsDTO
genOrganizationCreationParamsDTO n =
  OrganizationCreationParamsDTO
    <$> arbitrary -- organizationCreationParamsDTOName :: Text
    <*> arbitraryReduced n -- organizationCreationParamsDTOOrganizationType :: OrganizationTypeDTO
    <*> arbitraryReducedMaybe n -- organizationCreationParamsDTODiscountCode :: Maybe DiscountCodeDTO
  
instance Arbitrary OrganizationDTO where
  arbitrary = sized genOrganizationDTO

genOrganizationDTO :: Int -> Gen OrganizationDTO
genOrganizationDTO n =
  OrganizationDTO
    <$> arbitrary -- organizationDTOName :: Text
    <*> arbitrary -- organizationDTOPaymentStatus :: Text
    <*> arbitrary -- organizationDTOAvatarUrl :: Text
    <*> arbitraryReduced n -- organizationDTOOrganizationType :: OrganizationTypeDTO
    <*> arbitraryReduced n -- organizationDTOAvatarSource :: AvatarSourceEnum
    <*> arbitraryReducedMaybe n -- organizationDTOInfo :: Maybe Text
    <*> arbitrary -- organizationDTOId :: Text
    <*> arbitraryReduced n -- organizationDTOCreated :: DateTime
  
instance Arbitrary OrganizationInvitationDTO where
  arbitrary = sized genOrganizationInvitationDTO

genOrganizationInvitationDTO :: Int -> Gen OrganizationInvitationDTO
genOrganizationInvitationDTO n =
  OrganizationInvitationDTO
    <$> arbitraryReduced n -- organizationInvitationDTORoleGrant :: OrganizationRoleDTO
    <*> arbitrary -- organizationInvitationDTOInvitedBy :: Text
    <*> arbitrary -- organizationInvitationDTOOrganizationName :: Text
    <*> arbitrary -- organizationInvitationDTOId :: Text
    <*> arbitrary -- organizationInvitationDTOInvitee :: Text
    <*> arbitraryReduced n -- organizationInvitationDTOStatus :: InvitationStatusEnumDTO
    <*> arbitraryReduced n -- organizationInvitationDTOInvitationType :: InvitationTypeEnumDTO
  
instance Arbitrary OrganizationInvitationUpdateDTO where
  arbitrary = sized genOrganizationInvitationUpdateDTO

genOrganizationInvitationUpdateDTO :: Int -> Gen OrganizationInvitationUpdateDTO
genOrganizationInvitationUpdateDTO n =
  OrganizationInvitationUpdateDTO
    <$> arbitraryReduced n -- organizationInvitationUpdateDTORoleGrant :: OrganizationRoleDTO
  
instance Arbitrary OrganizationLimitsDTO where
  arbitrary = sized genOrganizationLimitsDTO

genOrganizationLimitsDTO :: Int -> Gen OrganizationLimitsDTO
genOrganizationLimitsDTO n =
  OrganizationLimitsDTO
    <$> arbitraryReducedMaybe n -- organizationLimitsDTOStorageSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- organizationLimitsDTOPrivateProjectMembers :: Maybe Integer
    <*> arbitraryReducedMaybe n -- organizationLimitsDTOProjectsLimit :: Maybe Integer
    <*> arbitraryReducedMaybe n -- organizationLimitsDTOMembersLimit :: Maybe Integer
  
instance Arbitrary OrganizationMemberDTO where
  arbitrary = sized genOrganizationMemberDTO

genOrganizationMemberDTO :: Int -> Gen OrganizationMemberDTO
genOrganizationMemberDTO n =
  OrganizationMemberDTO
    <$> arbitraryReduced n -- organizationMemberDTORole :: OrganizationRoleDTO
    <*> arbitrary -- organizationMemberDTOEditableRole :: Bool
    <*> arbitraryReducedMaybe n -- organizationMemberDTORegisteredMemberInfo :: Maybe RegisteredMemberInfoDTO
    <*> arbitraryReducedMaybe n -- organizationMemberDTOInvitationInfo :: Maybe OrganizationInvitationDTO
    <*> arbitraryReducedMaybe n -- organizationMemberDTOTotalNumberOfProjects :: Maybe Int
    <*> arbitraryReducedMaybe n -- organizationMemberDTONumberOfProjects :: Maybe Int
  
instance Arbitrary OrganizationMemberUpdateDTO where
  arbitrary = sized genOrganizationMemberUpdateDTO

genOrganizationMemberUpdateDTO :: Int -> Gen OrganizationMemberUpdateDTO
genOrganizationMemberUpdateDTO n =
  OrganizationMemberUpdateDTO
    <$> arbitraryReduced n -- organizationMemberUpdateDTORole :: OrganizationRoleDTO
  
instance Arbitrary OrganizationNameAvailableDTO where
  arbitrary = sized genOrganizationNameAvailableDTO

genOrganizationNameAvailableDTO :: Int -> Gen OrganizationNameAvailableDTO
genOrganizationNameAvailableDTO n =
  OrganizationNameAvailableDTO
    <$> arbitrary -- organizationNameAvailableDTOAvailable :: Bool
  
instance Arbitrary OrganizationPricingPlanDTO where
  arbitrary = sized genOrganizationPricingPlanDTO

genOrganizationPricingPlanDTO :: Int -> Gen OrganizationPricingPlanDTO
genOrganizationPricingPlanDTO n =
  OrganizationPricingPlanDTO
    <$> arbitraryReduced n -- organizationPricingPlanDTOPricingPlan :: PricingPlanDTO
  
instance Arbitrary OrganizationUpdateDTO where
  arbitrary = sized genOrganizationUpdateDTO

genOrganizationUpdateDTO :: Int -> Gen OrganizationUpdateDTO
genOrganizationUpdateDTO n =
  OrganizationUpdateDTO
    <$> arbitraryReducedMaybe n -- organizationUpdateDTOName :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationUpdateDTOInfo :: Maybe Text
  
instance Arbitrary OrganizationWithRoleDTO where
  arbitrary = sized genOrganizationWithRoleDTO

genOrganizationWithRoleDTO :: Int -> Gen OrganizationWithRoleDTO
genOrganizationWithRoleDTO n =
  OrganizationWithRoleDTO
    <$> arbitrary -- organizationWithRoleDTOName :: Text
    <*> arbitraryReducedMaybe n -- organizationWithRoleDTOUserRole :: Maybe OrganizationRoleDTO
    <*> arbitrary -- organizationWithRoleDTOPaymentStatus :: Text
    <*> arbitrary -- organizationWithRoleDTOAvatarUrl :: Text
    <*> arbitraryReduced n -- organizationWithRoleDTOOrganizationType :: OrganizationTypeDTO
    <*> arbitraryReduced n -- organizationWithRoleDTOAvatarSource :: AvatarSourceEnum
    <*> arbitraryReducedMaybe n -- organizationWithRoleDTOInfo :: Maybe Text
    <*> arbitrary -- organizationWithRoleDTOId :: Text
    <*> arbitraryReduced n -- organizationWithRoleDTOCreated :: DateTime
  
instance Arbitrary OutputImageDTO where
  arbitrary = sized genOutputImageDTO

genOutputImageDTO :: Int -> Gen OutputImageDTO
genOutputImageDTO n =
  OutputImageDTO
    <$> arbitraryReducedMaybe n -- outputImageDTOName :: Maybe Text
    <*> arbitraryReducedMaybe n -- outputImageDTODescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- outputImageDTOImageUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- outputImageDTOThumbnailUrl :: Maybe Text
  
instance Arbitrary Parameter where
  arbitrary = sized genParameter

genParameter :: Int -> Gen Parameter
genParameter n =
  Parameter
    <$> arbitrary -- parameterName :: Text
    <*> arbitraryReducedMaybe n -- parameterDescription :: Maybe Text
    <*> arbitraryReduced n -- parameterParameterType :: ParameterTypeEnum
    <*> arbitrary -- parameterId :: Text
    <*> arbitrary -- parameterValue :: Text
  
instance Arbitrary PasswordChangeDTO where
  arbitrary = sized genPasswordChangeDTO

genPasswordChangeDTO :: Int -> Gen PasswordChangeDTO
genPasswordChangeDTO n =
  PasswordChangeDTO
    <$> arbitrary -- passwordChangeDTOCurrentPassword :: Text
    <*> arbitrary -- passwordChangeDTONewPassword :: Text
  
instance Arbitrary Point where
  arbitrary = sized genPoint

genPoint :: Int -> Gen Point
genPoint n =
  Point
    <$> arbitrary -- pointTimestampMillis :: Integer
    <*> arbitraryReducedMaybe n -- pointX :: Maybe Double
    <*> arbitraryReduced n -- pointY :: Y
  
instance Arbitrary PointValueDTO where
  arbitrary = sized genPointValueDTO

genPointValueDTO :: Int -> Gen PointValueDTO
genPointValueDTO n =
  PointValueDTO
    <$> arbitrary -- pointValueDTOTimestampMillis :: Integer
    <*> arbitrary -- pointValueDTOX :: Double
    <*> arbitraryReduced n -- pointValueDTOY :: Y
  
instance Arbitrary ProjectInvitationDTO where
  arbitrary = sized genProjectInvitationDTO

genProjectInvitationDTO :: Int -> Gen ProjectInvitationDTO
genProjectInvitationDTO n =
  ProjectInvitationDTO
    <$> arbitraryReduced n -- projectInvitationDTORoleGrant :: ProjectRoleDTO
    <*> arbitrary -- projectInvitationDTOProjectName :: Text
    <*> arbitrary -- projectInvitationDTOInvitedBy :: Text
    <*> arbitrary -- projectInvitationDTOOrganizationName :: Text
    <*> arbitrary -- projectInvitationDTOId :: Text
    <*> arbitrary -- projectInvitationDTOInvitee :: Text
    <*> arbitraryReduced n -- projectInvitationDTOStatus :: InvitationStatusEnumDTO
    <*> arbitraryReduced n -- projectInvitationDTOInvitationType :: InvitationTypeEnumDTO
  
instance Arbitrary ProjectInvitationUpdateDTO where
  arbitrary = sized genProjectInvitationUpdateDTO

genProjectInvitationUpdateDTO :: Int -> Gen ProjectInvitationUpdateDTO
genProjectInvitationUpdateDTO n =
  ProjectInvitationUpdateDTO
    <$> arbitraryReduced n -- projectInvitationUpdateDTORoleGrant :: ProjectRoleDTO
  
instance Arbitrary ProjectKeyDTO where
  arbitrary = sized genProjectKeyDTO

genProjectKeyDTO :: Int -> Gen ProjectKeyDTO
genProjectKeyDTO n =
  ProjectKeyDTO
    <$> arbitrary -- projectKeyDTOProposal :: Text
  
instance Arbitrary ProjectListDTO where
  arbitrary = sized genProjectListDTO

genProjectListDTO :: Int -> Gen ProjectListDTO
genProjectListDTO n =
  ProjectListDTO
    <$> arbitraryReduced n -- projectListDTOEntries :: [ProjectWithRoleDTO]
    <*> arbitrary -- projectListDTOMatchingItemCount :: Int
    <*> arbitrary -- projectListDTOTotalItemCount :: Int
  
instance Arbitrary ProjectMemberDTO where
  arbitrary = sized genProjectMemberDTO

genProjectMemberDTO :: Int -> Gen ProjectMemberDTO
genProjectMemberDTO n =
  ProjectMemberDTO
    <$> arbitraryReduced n -- projectMemberDTORole :: ProjectRoleDTO
    <*> arbitraryReducedMaybe n -- projectMemberDTORegisteredMemberInfo :: Maybe RegisteredMemberInfoDTO
    <*> arbitraryReducedMaybe n -- projectMemberDTOInvitationInfo :: Maybe ProjectInvitationDTO
    <*> arbitrary -- projectMemberDTOEditableRole :: Bool
    <*> arbitrary -- projectMemberDTOCanLeaveProject :: Bool
  
instance Arbitrary ProjectMemberUpdateDTO where
  arbitrary = sized genProjectMemberUpdateDTO

genProjectMemberUpdateDTO :: Int -> Gen ProjectMemberUpdateDTO
genProjectMemberUpdateDTO n =
  ProjectMemberUpdateDTO
    <$> arbitraryReduced n -- projectMemberUpdateDTORole :: ProjectRoleDTO
  
instance Arbitrary ProjectMembersDTO where
  arbitrary = sized genProjectMembersDTO

genProjectMembersDTO :: Int -> Gen ProjectMembersDTO
genProjectMembersDTO n =
  ProjectMembersDTO
    <$> arbitrary -- projectMembersDTOProjectName :: Text
    <*> arbitrary -- projectMembersDTOProjectId :: Text
    <*> arbitrary -- projectMembersDTOOrganizationName :: Text
    <*> arbitraryReduced n -- projectMembersDTOMembers :: [ProjectMemberDTO]
    <*> arbitrary -- projectMembersDTOOrganizationId :: Text
  
instance Arbitrary ProjectMetadataDTO where
  arbitrary = sized genProjectMetadataDTO

genProjectMetadataDTO :: Int -> Gen ProjectMetadataDTO
genProjectMetadataDTO n =
  ProjectMetadataDTO
    <$> arbitrary -- projectMetadataDTOName :: Text
    <*> arbitraryReduced n -- projectMetadataDTOOrganizationType :: OrganizationTypeDTO
    <*> arbitraryReduced n -- projectMetadataDTOTimeOfCreation :: DateTime
    <*> arbitrary -- projectMetadataDTOOrganizationName :: Text
    <*> arbitrary -- projectMetadataDTOVersion :: Int
    <*> arbitrary -- projectMetadataDTOId :: Text
    <*> arbitrary -- projectMetadataDTOProjectKey :: Text
    <*> arbitrary -- projectMetadataDTOOrganizationId :: Text
  
instance Arbitrary ProjectUpdateDTO where
  arbitrary = sized genProjectUpdateDTO

genProjectUpdateDTO :: Int -> Gen ProjectUpdateDTO
genProjectUpdateDTO n =
  ProjectUpdateDTO
    <$> arbitraryReducedMaybe n -- projectUpdateDTOCodeAccess :: Maybe ProjectCodeAccessDTO
    <*> arbitraryReducedMaybe n -- projectUpdateDTOName :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectUpdateDTODescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- projectUpdateDTOVisibility :: Maybe ProjectVisibilityDTO
    <*> arbitraryReducedMaybe n -- projectUpdateDTODisplayClass :: Maybe Text
  
instance Arbitrary ProjectWithRoleDTO where
  arbitrary = sized genProjectWithRoleDTO

genProjectWithRoleDTO :: Int -> Gen ProjectWithRoleDTO
genProjectWithRoleDTO n =
  ProjectWithRoleDTO
    <$> arbitraryReduced n -- projectWithRoleDTOCodeAccess :: ProjectCodeAccessDTO
    <*> arbitrary -- projectWithRoleDTOAvatarUrl :: Text
    <*> arbitraryReducedMaybe n -- projectWithRoleDTODescription :: Maybe Text
    <*> arbitraryReduced n -- projectWithRoleDTOOrganizationType :: OrganizationTypeDTO
    <*> arbitrary -- projectWithRoleDTOFeatured :: Bool
    <*> arbitrary -- projectWithRoleDTOOrganizationName :: Text
    <*> arbitrary -- projectWithRoleDTOVersion :: Int
    <*> arbitrary -- projectWithRoleDTOId :: Text
    <*> arbitrary -- projectWithRoleDTOProjectKey :: Text
    <*> arbitrary -- projectWithRoleDTOOrganizationId :: Text
    <*> arbitrary -- projectWithRoleDTOUserCount :: Int
    <*> arbitraryReduced n -- projectWithRoleDTOVisibility :: ProjectVisibilityDTO
    <*> arbitraryReducedMaybe n -- projectWithRoleDTODisplayClass :: Maybe Text
    <*> arbitrary -- projectWithRoleDTOName :: Text
    <*> arbitraryReduced n -- projectWithRoleDTOLastActivity :: DateTime
    <*> arbitraryReduced n -- projectWithRoleDTOTimeOfCreation :: DateTime
    <*> arbitraryReducedMaybe n -- projectWithRoleDTOUserRoleInOrganization :: Maybe OrganizationRoleDTO
    <*> arbitraryReduced n -- projectWithRoleDTOAvatarSource :: AvatarSourceEnum
    <*> arbitrary -- projectWithRoleDTOLeaderboardEntryCount :: Int
    <*> arbitraryReduced n -- projectWithRoleDTOUserRoleInProject :: ProjectRoleDTO
    <*> arbitraryReducedMaybe n -- projectWithRoleDTOBackgroundUrl :: Maybe Text
  
instance Arbitrary PublicUserProfileDTO where
  arbitrary = sized genPublicUserProfileDTO

genPublicUserProfileDTO :: Int -> Gen PublicUserProfileDTO
genPublicUserProfileDTO n =
  PublicUserProfileDTO
    <$> arbitrary -- publicUserProfileDTOBiography :: Text
    <*> arbitraryReducedMaybe n -- publicUserProfileDTOEmail :: Maybe Text
    <*> arbitraryReduced n -- publicUserProfileDTOAvatarSource :: AvatarSourceEnum
    <*> arbitraryReducedMaybe n -- publicUserProfileDTOFirstName :: Maybe Text
    <*> arbitrary -- publicUserProfileDTOShortInfo :: Text
    <*> arbitrary -- publicUserProfileDTOUsername :: Text
    <*> arbitrary -- publicUserProfileDTOAvatarUrl :: Text
    <*> arbitraryReducedMaybe n -- publicUserProfileDTOLastName :: Maybe Text
    <*> arbitraryReduced n -- publicUserProfileDTOLinks :: UserProfileLinksDTO
  
instance Arbitrary QuestionnaireDTO where
  arbitrary = sized genQuestionnaireDTO

genQuestionnaireDTO :: Int -> Gen QuestionnaireDTO
genQuestionnaireDTO n =
  QuestionnaireDTO
    <$> arbitrary -- questionnaireDTOAnswers :: Text
  
instance Arbitrary RegisteredMemberInfoDTO where
  arbitrary = sized genRegisteredMemberInfoDTO

genRegisteredMemberInfoDTO :: Int -> Gen RegisteredMemberInfoDTO
genRegisteredMemberInfoDTO n =
  RegisteredMemberInfoDTO
    <$> arbitraryReduced n -- registeredMemberInfoDTOAvatarSource :: AvatarSourceEnum
    <*> arbitrary -- registeredMemberInfoDTOLastName :: Text
    <*> arbitrary -- registeredMemberInfoDTOFirstName :: Text
    <*> arbitrary -- registeredMemberInfoDTOUsername :: Text
    <*> arbitrary -- registeredMemberInfoDTOAvatarUrl :: Text
  
instance Arbitrary RegistrationSurveyDTO where
  arbitrary = sized genRegistrationSurveyDTO

genRegistrationSurveyDTO :: Int -> Gen RegistrationSurveyDTO
genRegistrationSurveyDTO n =
  RegistrationSurveyDTO
    <$> arbitrary -- registrationSurveyDTOSurvey :: Text
  
instance Arbitrary Series where
  arbitrary = sized genSeries

genSeries :: Int -> Gen Series
genSeries n =
  Series
    <$> arbitraryReduced n -- seriesSeriesType :: SeriesType
    <*> arbitraryReducedMaybe n -- seriesChannelName :: Maybe Text
    <*> arbitraryReducedMaybe n -- seriesChannelId :: Maybe Text
    <*> arbitraryReducedMaybe n -- seriesAliasId :: Maybe Text
    <*> arbitrary -- seriesLabel :: Text
  
instance Arbitrary SeriesDefinition where
  arbitrary = sized genSeriesDefinition

genSeriesDefinition :: Int -> Gen SeriesDefinition
genSeriesDefinition n =
  SeriesDefinition
    <$> arbitrary -- seriesDefinitionLabel :: Text
    <*> arbitraryReducedMaybe n -- seriesDefinitionChannelName :: Maybe Text
    <*> arbitraryReducedMaybe n -- seriesDefinitionAliasId :: Maybe Text
    <*> arbitraryReduced n -- seriesDefinitionSeriesType :: SeriesType
  
instance Arbitrary SessionDTO where
  arbitrary = sized genSessionDTO

genSessionDTO :: Int -> Gen SessionDTO
genSessionDTO n =
  SessionDTO
    <$> arbitrary -- sessionDTOSessionId :: Text
  
instance Arbitrary StateTransitions where
  arbitrary = sized genStateTransitions

genStateTransitions :: Int -> Gen StateTransitions
genStateTransitions n =
  StateTransitions
    <$> arbitraryReducedMaybe n -- stateTransitionsRunning :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- stateTransitionsSucceeded :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- stateTransitionsFailed :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- stateTransitionsAborted :: Maybe DateTime
  
instance Arbitrary StorageUsage where
  arbitrary = sized genStorageUsage

genStorageUsage :: Int -> Gen StorageUsage
genStorageUsage n =
  StorageUsage
    <$> arbitrary -- storageUsageUsage :: Integer
  
instance Arbitrary SubscriptionCancelInfoDTO where
  arbitrary = sized genSubscriptionCancelInfoDTO

genSubscriptionCancelInfoDTO :: Int -> Gen SubscriptionCancelInfoDTO
genSubscriptionCancelInfoDTO n =
  SubscriptionCancelInfoDTO
    <$> arbitrary -- subscriptionCancelInfoDTOReasons :: [Text]
    <*> arbitraryReducedMaybe n -- subscriptionCancelInfoDTODescription :: Maybe Text
  
instance Arbitrary SystemMetric where
  arbitrary = sized genSystemMetric

genSystemMetric :: Int -> Gen SystemMetric
genSystemMetric n =
  SystemMetric
    <$> arbitrary -- systemMetricSeries :: [Text]
    <*> arbitrary -- systemMetricName :: Text
    <*> arbitraryReducedMaybe n -- systemMetricMin :: Maybe Double
    <*> arbitraryReducedMaybe n -- systemMetricMax :: Maybe Double
    <*> arbitraryReducedMaybe n -- systemMetricUnit :: Maybe Text
    <*> arbitrary -- systemMetricDescription :: Text
    <*> arbitraryReduced n -- systemMetricResourceType :: SystemMetricResourceType
    <*> arbitrary -- systemMetricExperimentId :: Text
    <*> arbitrary -- systemMetricId :: Text
  
instance Arbitrary SystemMetricParams where
  arbitrary = sized genSystemMetricParams

genSystemMetricParams :: Int -> Gen SystemMetricParams
genSystemMetricParams n =
  SystemMetricParams
    <$> arbitrary -- systemMetricParamsSeries :: [Text]
    <*> arbitrary -- systemMetricParamsName :: Text
    <*> arbitraryReducedMaybe n -- systemMetricParamsMin :: Maybe Double
    <*> arbitraryReducedMaybe n -- systemMetricParamsMax :: Maybe Double
    <*> arbitraryReducedMaybe n -- systemMetricParamsUnit :: Maybe Text
    <*> arbitrary -- systemMetricParamsDescription :: Text
    <*> arbitraryReduced n -- systemMetricParamsResourceType :: SystemMetricResourceType
  
instance Arbitrary SystemMetricPoint where
  arbitrary = sized genSystemMetricPoint

genSystemMetricPoint :: Int -> Gen SystemMetricPoint
genSystemMetricPoint n =
  SystemMetricPoint
    <$> arbitrary -- systemMetricPointTimestampMillis :: Integer
    <*> arbitrary -- systemMetricPointX :: Integer
    <*> arbitrary -- systemMetricPointY :: Double
  
instance Arbitrary SystemMetricValues where
  arbitrary = sized genSystemMetricValues

genSystemMetricValues :: Int -> Gen SystemMetricValues
genSystemMetricValues n =
  SystemMetricValues
    <$> arbitrary -- systemMetricValuesMetricId :: Text
    <*> arbitrary -- systemMetricValuesSeriesName :: Text
    <*> arbitraryReducedMaybe n -- systemMetricValuesLevel :: Maybe Int
    <*> arbitraryReduced n -- systemMetricValuesValues :: [SystemMetricPoint]
  
instance Arbitrary UUID where
  arbitrary = sized genUUID

genUUID :: Int -> Gen UUID
genUUID n =
  UUID
    <$> arbitrary -- uUIDMostSigBits :: Integer
    <*> arbitrary -- uUIDLeastSigBits :: Integer
  
instance Arbitrary UpdateTagsParams where
  arbitrary = sized genUpdateTagsParams

genUpdateTagsParams :: Int -> Gen UpdateTagsParams
genUpdateTagsParams n =
  UpdateTagsParams
    <$> arbitrary -- updateTagsParamsExperimentIds :: [Text]
    <*> arbitrary -- updateTagsParamsTagsToAdd :: [Text]
    <*> arbitrary -- updateTagsParamsTagsToDelete :: [Text]
  
instance Arbitrary UserListDTO where
  arbitrary = sized genUserListDTO

genUserListDTO :: Int -> Gen UserListDTO
genUserListDTO n =
  UserListDTO
    <$> arbitraryReduced n -- userListDTOEntries :: [UserListItemDTO]
    <*> arbitrary -- userListDTOMatchingItemCount :: Int
    <*> arbitrary -- userListDTOTotalItemCount :: Int
  
instance Arbitrary UserListItemDTO where
  arbitrary = sized genUserListItemDTO

genUserListItemDTO :: Int -> Gen UserListItemDTO
genUserListItemDTO n =
  UserListItemDTO
    <$> arbitraryReduced n -- userListItemDTOAvatarSource :: AvatarSourceEnum
    <*> arbitrary -- userListItemDTOLastName :: Text
    <*> arbitrary -- userListItemDTOFirstName :: Text
    <*> arbitrary -- userListItemDTOUsername :: Text
    <*> arbitrary -- userListItemDTOAvatarUrl :: Text
  
instance Arbitrary UserPricingStatusDTO where
  arbitrary = sized genUserPricingStatusDTO

genUserPricingStatusDTO :: Int -> Gen UserPricingStatusDTO
genUserPricingStatusDTO n =
  UserPricingStatusDTO
    <$> arbitrary -- userPricingStatusDTOCanCreateTeamFree :: Bool
    <*> arbitraryReducedMaybe n -- userPricingStatusDTOAnyTeamFree :: Maybe OrganizationWithRoleDTO
  
instance Arbitrary UserProfileDTO where
  arbitrary = sized genUserProfileDTO

genUserProfileDTO :: Int -> Gen UserProfileDTO
genUserProfileDTO n =
  UserProfileDTO
    <$> arbitrary -- userProfileDTOUsernameHash :: Text
    <*> arbitrary -- userProfileDTOEmail :: Text
    <*> arbitrary -- userProfileDTOHasLoggedToCli :: Bool
    <*> arbitraryReduced n -- userProfileDTOAvatarSource :: AvatarSourceEnum
    <*> arbitrary -- userProfileDTOFirstName :: Text
    <*> arbitrary -- userProfileDTOShortInfo :: Text
    <*> arbitraryReduced n -- userProfileDTOCreated :: DateTime
    <*> arbitrary -- userProfileDTOBiography :: Text
    <*> arbitrary -- userProfileDTOHasCreatedExperiments :: Bool
    <*> arbitrary -- userProfileDTOUsername :: Text
    <*> arbitrary -- userProfileDTOAvatarUrl :: Text
    <*> arbitrary -- userProfileDTOLastName :: Text
    <*> arbitraryReduced n -- userProfileDTOLinks :: UserProfileLinksDTO
  
instance Arbitrary UserProfileLinkDTO where
  arbitrary = sized genUserProfileLinkDTO

genUserProfileLinkDTO :: Int -> Gen UserProfileLinkDTO
genUserProfileLinkDTO n =
  UserProfileLinkDTO
    <$> arbitraryReduced n -- userProfileLinkDTOLinkType :: LinkTypeDTO
    <*> arbitrary -- userProfileLinkDTOUrl :: Text
  
instance Arbitrary UserProfileLinksDTO where
  arbitrary = sized genUserProfileLinksDTO

genUserProfileLinksDTO :: Int -> Gen UserProfileLinksDTO
genUserProfileLinksDTO n =
  UserProfileLinksDTO
    <$> arbitraryReducedMaybe n -- userProfileLinksDTOGithub :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileLinksDTOLinkedin :: Maybe Text
    <*> arbitrary -- userProfileLinksDTOOthers :: [Text]
    <*> arbitraryReducedMaybe n -- userProfileLinksDTOKaggle :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileLinksDTOTwitter :: Maybe Text
  
instance Arbitrary UserProfileUpdateDTO where
  arbitrary = sized genUserProfileUpdateDTO

genUserProfileUpdateDTO :: Int -> Gen UserProfileUpdateDTO
genUserProfileUpdateDTO n =
  UserProfileUpdateDTO
    <$> arbitraryReducedMaybe n -- userProfileUpdateDTOBiography :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileUpdateDTOHasLoggedToCli :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userProfileUpdateDTOLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileUpdateDTOFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileUpdateDTOShortInfo :: Maybe Text
  
instance Arbitrary UsernameValidationStatusDTO where
  arbitrary = sized genUsernameValidationStatusDTO

genUsernameValidationStatusDTO :: Int -> Gen UsernameValidationStatusDTO
genUsernameValidationStatusDTO n =
  UsernameValidationStatusDTO
    <$> arbitraryReduced n -- usernameValidationStatusDTOStatus :: UsernameValidationStatusEnumDTO
  
instance Arbitrary Version where
  arbitrary = sized genVersion

genVersion :: Int -> Gen Version
genVersion n =
  Version
    <$> arbitrary -- versionVersion :: Text
  
instance Arbitrary WorkspaceConfig where
  arbitrary = sized genWorkspaceConfig

genWorkspaceConfig :: Int -> Gen WorkspaceConfig
genWorkspaceConfig n =
  WorkspaceConfig
    <$> arbitrary -- workspaceConfigRealm :: Text
    <*> arbitrary -- workspaceConfigIdpHint :: Text
  
instance Arbitrary Y where
  arbitrary = sized genY

genY :: Int -> Gen Y
genY n =
  Y
    <$> arbitraryReducedMaybe n -- yNumericValue :: Maybe Double
    <*> arbitraryReducedMaybe n -- yTextValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- yImageValue :: Maybe OutputImageDTO
    <*> arbitraryReducedMaybe n -- yInputImageValue :: Maybe InputImageDTO
  



instance Arbitrary AchievementTypeDTO where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ApiErrorTypeDTO where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AvatarSourceEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ChannelType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ChannelTypeEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ExperimentState where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary InvitationStatusEnumDTO where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary InvitationTypeEnumDTO where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LinkTypeDTO where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LoginActionDTO where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary NameEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OrganizationRoleDTO where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OrganizationTypeDTO where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ParameterTypeEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PricingPlanDTO where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ProjectCodeAccessDTO where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ProjectRoleDTO where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ProjectVisibilityDTO where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SeriesType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SystemMetricResourceType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary UsernameValidationStatusEnumDTO where
  arbitrary = arbitraryBoundedEnum

