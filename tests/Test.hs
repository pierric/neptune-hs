{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import NeptuneBackend.Model
import NeptuneBackend.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy AchievementTypeDTO)
      propMimeEq MimeJSON (Proxy :: Proxy AchievementsDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ApiErrorTypeDTO)
      propMimeEq MimeJSON (Proxy :: Proxy AuthorizedUserDTO)
      propMimeEq MimeJSON (Proxy :: Proxy AvatarSourceEnum)
      propMimeEq MimeJSON (Proxy :: Proxy BatchChannelValueErrorDTO)
      propMimeEq MimeJSON (Proxy :: Proxy BatchExperimentUpdateResult)
      propMimeEq MimeJSON (Proxy :: Proxy Channel)
      propMimeEq MimeJSON (Proxy :: Proxy ChannelDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ChannelParams)
      propMimeEq MimeJSON (Proxy :: Proxy ChannelType)
      propMimeEq MimeJSON (Proxy :: Proxy ChannelTypeEnum)
      propMimeEq MimeJSON (Proxy :: Proxy ChannelWithValue)
      propMimeEq MimeJSON (Proxy :: Proxy ChannelWithValueDTO)
      propMimeEq MimeJSON (Proxy :: Proxy Chart)
      propMimeEq MimeJSON (Proxy :: Proxy ChartDefinition)
      propMimeEq MimeJSON (Proxy :: Proxy ChartSet)
      propMimeEq MimeJSON (Proxy :: Proxy ChartSetParams)
      propMimeEq MimeJSON (Proxy :: Proxy Charts)
      propMimeEq MimeJSON (Proxy :: Proxy ClientConfig)
      propMimeEq MimeJSON (Proxy :: Proxy ClientVersionsConfigDTO)
      propMimeEq MimeJSON (Proxy :: Proxy CompletedExperimentParams)
      propMimeEq MimeJSON (Proxy :: Proxy ComponentStatus)
      propMimeEq MimeJSON (Proxy :: Proxy ComponentVersion)
      propMimeEq MimeJSON (Proxy :: Proxy ConfigInfo)
      propMimeEq MimeJSON (Proxy :: Proxy CreateSessionParamsDTO)
      propMimeEq MimeJSON (Proxy :: Proxy CustomerDTO)
      propMimeEq MimeJSON (Proxy :: Proxy DiscountCodeDTO)
      propMimeEq MimeJSON (Proxy :: Proxy DiscountDTO)
      propMimeEq MimeJSON (Proxy :: Proxy DownloadPrepareRequestDTO)
      propMimeEq MimeJSON (Proxy :: Proxy EditExperimentParams)
      propMimeEq MimeJSON (Proxy :: Proxy Error)
      propMimeEq MimeJSON (Proxy :: Proxy Experiment)
      propMimeEq MimeJSON (Proxy :: Proxy ExperimentCreationParams)
      propMimeEq MimeJSON (Proxy :: Proxy ExperimentPaths)
      propMimeEq MimeJSON (Proxy :: Proxy ExperimentState)
      propMimeEq MimeJSON (Proxy :: Proxy ExperimentsAttributesNamesDTO)
      propMimeEq MimeJSON (Proxy :: Proxy File)
      propMimeEq MimeJSON (Proxy :: Proxy GitCommitDTO)
      propMimeEq MimeJSON (Proxy :: Proxy GitInfoDTO)
      propMimeEq MimeJSON (Proxy :: Proxy GlobalConfiguration)
      propMimeEq MimeJSON (Proxy :: Proxy InputChannelValues)
      propMimeEq MimeJSON (Proxy :: Proxy InputImageDTO)
      propMimeEq MimeJSON (Proxy :: Proxy InputMetadata)
      propMimeEq MimeJSON (Proxy :: Proxy InvitationStatusEnumDTO)
      propMimeEq MimeJSON (Proxy :: Proxy InvitationTypeEnumDTO)
      propMimeEq MimeJSON (Proxy :: Proxy KeyValueProperty)
      propMimeEq MimeJSON (Proxy :: Proxy LimitedChannelValuesDTO)
      propMimeEq MimeJSON (Proxy :: Proxy Link)
      propMimeEq MimeJSON (Proxy :: Proxy LinkDTO)
      propMimeEq MimeJSON (Proxy :: Proxy LinkTypeDTO)
      propMimeEq MimeJSON (Proxy :: Proxy LoginActionDTO)
      propMimeEq MimeJSON (Proxy :: Proxy LoginActionsListDTO)
      propMimeEq MimeJSON (Proxy :: Proxy NameEnum)
      propMimeEq MimeJSON (Proxy :: Proxy NeptuneApiToken)
      propMimeEq MimeJSON (Proxy :: Proxy NeptuneOauthToken)
      propMimeEq MimeJSON (Proxy :: Proxy NewOrganizationInvitationDTO)
      propMimeEq MimeJSON (Proxy :: Proxy NewOrganizationMemberDTO)
      propMimeEq MimeJSON (Proxy :: Proxy NewProjectDTO)
      propMimeEq MimeJSON (Proxy :: Proxy NewProjectInvitationDTO)
      propMimeEq MimeJSON (Proxy :: Proxy NewProjectMemberDTO)
      propMimeEq MimeJSON (Proxy :: Proxy NewReservationDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationCreationParamsDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationInvitationDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationInvitationUpdateDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationLimitsDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationMemberDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationMemberUpdateDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationNameAvailableDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationPricingPlanDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationRoleDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationTypeDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationUpdateDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationWithRoleDTO)
      propMimeEq MimeJSON (Proxy :: Proxy OutputImageDTO)
      propMimeEq MimeJSON (Proxy :: Proxy Parameter)
      propMimeEq MimeJSON (Proxy :: Proxy ParameterTypeEnum)
      propMimeEq MimeJSON (Proxy :: Proxy PasswordChangeDTO)
      propMimeEq MimeJSON (Proxy :: Proxy Point)
      propMimeEq MimeJSON (Proxy :: Proxy PointValueDTO)
      propMimeEq MimeJSON (Proxy :: Proxy PricingPlanDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectCodeAccessDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectInvitationDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectInvitationUpdateDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectKeyDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectListDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectMemberDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectMemberUpdateDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectMembersDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectMetadataDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectRoleDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectUpdateDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectVisibilityDTO)
      propMimeEq MimeJSON (Proxy :: Proxy ProjectWithRoleDTO)
      propMimeEq MimeJSON (Proxy :: Proxy PublicUserProfileDTO)
      propMimeEq MimeJSON (Proxy :: Proxy QuestionnaireDTO)
      propMimeEq MimeJSON (Proxy :: Proxy RegisteredMemberInfoDTO)
      propMimeEq MimeJSON (Proxy :: Proxy RegistrationSurveyDTO)
      propMimeEq MimeJSON (Proxy :: Proxy Series)
      propMimeEq MimeJSON (Proxy :: Proxy SeriesDefinition)
      propMimeEq MimeJSON (Proxy :: Proxy SeriesType)
      propMimeEq MimeJSON (Proxy :: Proxy SessionDTO)
      propMimeEq MimeJSON (Proxy :: Proxy StateTransitions)
      propMimeEq MimeJSON (Proxy :: Proxy StorageUsage)
      propMimeEq MimeJSON (Proxy :: Proxy SubscriptionCancelInfoDTO)
      propMimeEq MimeJSON (Proxy :: Proxy SystemMetric)
      propMimeEq MimeJSON (Proxy :: Proxy SystemMetricParams)
      propMimeEq MimeJSON (Proxy :: Proxy SystemMetricPoint)
      propMimeEq MimeJSON (Proxy :: Proxy SystemMetricResourceType)
      propMimeEq MimeJSON (Proxy :: Proxy SystemMetricValues)
      propMimeEq MimeJSON (Proxy :: Proxy UUID)
      propMimeEq MimeJSON (Proxy :: Proxy UpdateTagsParams)
      propMimeEq MimeJSON (Proxy :: Proxy UserListDTO)
      propMimeEq MimeJSON (Proxy :: Proxy UserListItemDTO)
      propMimeEq MimeJSON (Proxy :: Proxy UserPricingStatusDTO)
      propMimeEq MimeJSON (Proxy :: Proxy UserProfileDTO)
      propMimeEq MimeJSON (Proxy :: Proxy UserProfileLinkDTO)
      propMimeEq MimeJSON (Proxy :: Proxy UserProfileLinksDTO)
      propMimeEq MimeJSON (Proxy :: Proxy UserProfileUpdateDTO)
      propMimeEq MimeJSON (Proxy :: Proxy UsernameValidationStatusDTO)
      propMimeEq MimeJSON (Proxy :: Proxy UsernameValidationStatusEnumDTO)
      propMimeEq MimeJSON (Proxy :: Proxy Version)
      propMimeEq MimeJSON (Proxy :: Proxy WorkspaceConfig)
      propMimeEq MimeJSON (Proxy :: Proxy Y)
      
