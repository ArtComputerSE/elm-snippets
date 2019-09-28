module DecryptTest exposing (someString, suite)

import Decrypt exposing (decrypt, encrypt)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Decrypt suite"
        [ test "Given encrypted string then the decrypted is the same." <|
            \() -> Expect.equal someString (decrypt <| encrypt someString)
        , test "Handle the empty string." <|
            \() -> Expect.equal "" (decrypt <| encrypt "")
        , test "Handle a long string." <|
            \() -> Expect.equal longString (decrypt <| encrypt longString)
        ]


someString : String
someString =
    "some string"


longString : String
longString =
    """
    {
      "$": 0,
      "a": {
        "metadata": {
          "versions": {
            "elm": "0.19.0"
          },
          "types": {
            "message": "Main.Msg",
            "aliases": {
              "Browser.Dom.Viewport": {
                "args": [],
                "type": "{ scene : { width : Basics.Float, height : Basics.Float }, viewport : { x : Basics.Float, y : Basics.Float, width : Basics.Float, height : Basics.Float } }"
              },
              "Chat.Chat.ChatInfo": {
                "args": [],
                "type": "{ creatorName : String.String, creatorSexYearOrientation : String.String, chatId : String.String, profilePhotoUrl : String.String, invited : List.List Chat.Chat.ChatParty, lastUpdate : Time.Posix, firstMessage : String.String }"
              },
              "Chat.Chat.ChatMessage": {
                "args": [],
                "type": "{ senderName : String.String, sendTime : Time.Posix, messageText : String.String }"
              },
              "Chat.Chat.ChatParty": {
                "args": [],
                "type": "{ name : String.String, lastRead : Time.Posix }"
              },
              "Event.GroupEvent.GroupEvent": {
                "args": [],
                "type": "{ id : String.String, authorUserName : String.String, groupName : String.String, description : String.String, place : String.String, eventTime : Time.Posix, myResponse : Event.GroupEvent.GroupEventResponse }"
              },
              "Event.GroupEvent.GroupEventParticipant": {
                "args": [],
                "type": "{ response : Event.GroupEvent.GroupEventResponse, profile : Profile.Profile.Profile }"
              },
              "Group.Group.GroupComment": {
                "args": [],
                "type": "{ id : String.String, userName : String.String, profilePhotoUrl : String.String, message : String.String, sendTime : Time.Posix, comments : List.List Group.Group.GroupReply }"
              },
              "Group.Group.GroupInfo": {
                "args": [],
                "type": "{ groupName : String.String, lastUpdate : Time.Posix, groupId : String.String }"
              },
              "Group.Group.GroupInvite": {
                "args": [],
                "type": "{ groupName : String.String, groupId : String.String, creatorName : String.String, message : String.String }"
              },
              "Group.Group.GroupMember": {
                "args": [],
                "type": "{ userName : String.String, profilePhotoUrl : String.String, admin : Basics.Bool }"
              },
              "Group.Group.GroupMessage": {
                "args": [],
                "type": "{ id : String.String, userName : String.String, profilePhotoUrl : String.String, message : String.String, sendTime : Time.Posix, lastUpdate : Time.Posix, comments : List.List Group.Group.GroupComment }"
              },
              "Group.Group.GroupReply": {
                "args": [],
                "type": "{ id : String.String, userName : String.String, profilePhotoUrl : String.String, message : String.String, sendTime : Time.Posix }"
              },
              "Profile.Photo.Photo": {
                "args": [],
                "type": "{ url : String.String, ordinal : Basics.Int, rotation : Basics.Int, height : Basics.Int, width : Basics.Int }"
              },
              "Profile.Profile.Member": {
                "args": [],
                "type": "{ profile : Profile.Profile.Profile, photos : List.List Profile.Photo.Photo }"
              },
              "Profile.Profile.Person": {
                "args": [],
                "type": "{ sex : Profile.ProfileAttributes.PersonSex, year : Maybe.Maybe Basics.Int, height : Maybe.Maybe Basics.Int, weight : Maybe.Maybe Basics.Int, body : Profile.ProfileAttributes.PersonBody, eyes : Profile.ProfileAttributes.PersonEyeColor, orientation : Profile.ProfileAttributes.PersonOrientation, smoking : Profile.ProfileAttributes.PersonHabit, drinking : Profile.ProfileAttributes.PersonHabit, martialStatus : Profile.ProfileAttributes.PersonMartialStatus, ethnicity : Profile.ProfileAttributes.PersonEthnicity, breastSize : Profile.ProfileAttributes.BreastSize, penisSize : Profile.ProfileAttributes.PenisSize, shavedStyle : Profile.ProfileAttributes.ShavedStyle }"
              },
              "Profile.Profile.Profile": {
                "args": [],
                "type": "{ userName : String.String, person1 : Profile.Profile.Person, person2 : Maybe.Maybe Profile.Profile.Person, description : String.String, profilePhotoUrl : String.String, cities : List.List String.String }"
              },
              "SharedState.LoginResponse": {
                "args": [],
                "type": "{ sessionId : String.String, userName : String.String }"
              },
              "SharedState.SessionVerificationResponse": {
                "args": [],
                "type": "{ sessionId : String.String, userName : String.String }"
              },
              "Time.Era": {
                "args": [],
                "type": "{ start : Basics.Int, offset : Basics.Int }"
              },
              "Url.Url": {
                "args": [],
                "type": "{ protocol : Url.Protocol, host : String.String, port_ : Maybe.Maybe Basics.Int, path : String.String, query : Maybe.Maybe String.String, fragment : Maybe.Maybe String.String }"
              }
            },
            "unions": {
              "Basics.Bool": {
                "args": [],
                "tags": {
                  "False": [],
                  "True": []
                }
              },
              "Basics.Float": {
                "args": [],
                "tags": {
                  "Float": []
                }
              },
              "Basics.Int": {
                "args": [],
                "tags": {
                  "Int": []
                }
              },
              "Browser.UrlRequest": {
                "args": [],
                "tags": {
                  "External": [
                    "String.String"
                  ],
                  "Internal": [
                    "Url.Url"
                  ]
                }
              },
              "Chat.Chat.ChatInviteStatus": {
                "args": [],
                "tags": {
                  "AlreadyChat": [],
                  "Ignored": [],
                  "Ignoring": [],
                  "Invited": [],
                  "MayInvite": [],
                  "PleaseAccept": []
                }
              },
              "Event.GroupEvent.GroupEventResponse": {
                "args": [],
                "tags": {
                  "Maybe": [],
                  "NoAnswer": [],
                  "Yes": []
                }
              },
              "File.File": {
                "args": [],
                "tags": {
                  "File": []
                }
              },
              "Http.Error": {
                "args": [],
                "tags": {
                  "BadBody": [
                    "String.String"
                  ],
                  "BadStatus": [
                    "Basics.Int"
                  ],
                  "BadUrl": [
                    "String.String"
                  ],
                  "NetworkError": [],
                  "Timeout": []
                }
              },
              "Http.Progress": {
                "args": [],
                "tags": {
                  "Receiving": [
                    "{ received : Basics.Int, size : Maybe.Maybe Basics.Int }"
                  ],
                  "Sending": [
                    "{ sent : Basics.Int, size : Basics.Int }"
                  ]
                }
              },
              "List.List": {
                "args": [
                  "a"
                ],
                "tags": {}
              },
              "Main.Msg": {
                "args": [],
                "tags": {
                  "Beat": [
                    "Time.Posix"
                  ],
                  "GotProgress": [
                    "Http.Progress"
                  ],
                  "LinkClicked": [
                    "Browser.UrlRequest"
                  ],
                  "ReceiveTimeZone": [
                    "Result.Result TimeZone.Error ( String.String, Time.Zone )"
                  ],
                  "RouterMsg": [
                    "Routing.Router.Msg"
                  ],
                  "SessionVerified": [
                    "Result.Result Http.Error SharedState.SessionVerificationResponse"
                  ],
                  "SetViewPort": [
                    "Browser.Dom.Viewport"
                  ],
                  "UrlChange": [
                    "Url.Url"
                  ],
                  "WindowResize": [
                    "Basics.Int",
                    "Basics.Int"
                  ]
                }
              },
              "Maybe.Maybe": {
                "args": [
                  "a"
                ],
                "tags": {
                  "Just": [
                    "a"
                  ],
                  "Nothing": []
                }
              },
              "Pages.ChatPage.Msg": {
                "args": [],
                "tags": {
                  "AcceptInvite": [
                    "String.String"
                  ],
                  "ChatReadSent": [
                    "Result.Result Http.Error ()"
                  ],
                  "ChatSelected": [
                    "Chat.Chat.ChatInfo"
                  ],
                  "ChatsLoaded": [
                    "Result.Result Http.Error (List.List Chat.Chat.ChatInfo)"
                  ],
                  "DeclineInvite": [
                    "String.String"
                  ],
                  "FetchProfile": [
                    "String.String"
                  ],
                  "Ignore": [
                    "String.String"
                  ],
                  "Ignored": [
                    "Result.Result Http.Error ()"
                  ],
                  "InvitesLoaded": [
                    "Result.Result Http.Error (List.List Chat.Chat.ChatInfo)"
                  ],
                  "LoadPage": [
                    "Maybe.Maybe String.String"
                  ],
                  "ManageInviteDone": [
                    "Result.Result Http.Error ()"
                  ],
                  "MessageSent": [
                    "Result.Result Http.Error ()"
                  ],
                  "MessageUpdate": [
                    "String.String"
                  ],
                  "MessagesLoaded": [
                    "Result.Result Http.Error (List.List Chat.Chat.ChatMessage)"
                  ],
                  "NewMessagesCountReset": [
                    "Result.Result Http.Error ()"
                  ],
                  "NoOp": [],
                  "SendMessage": [],
                  "ShowChatTab": [],
                  "ShowInvitesTab": []
                }
              },
              "Pages.EventPage.Msg": {
                "args": [],
                "tags": {
                  "ChangeEventDate": [
                    "String.String"
                  ],
                  "ChangeEventDescription": [
                    "String.String"
                  ],
                  "ChangeEventPlace": [
                    "String.String"
                  ],
                  "ChangeEventTime": [
                    "String.String"
                  ],
                  "CreateEvent": [],
                  "DeselectEvent": [],
                  "EventUpdated": [
                    "Result.Result Http.Error Event.GroupEvent.GroupEvent"
                  ],
                  "EventsLoaded": [
                    "Result.Result Http.Error (List.List Event.GroupEvent.GroupEvent)"
                  ],
                  "FetchProfile": [
                    "String.String"
                  ],
                  "GroupSelected": [
                    "Group.Group.GroupInfo"
                  ],
                  "GroupsLoaded": [
                    "Result.Result Http.Error (List.List Group.Group.GroupInfo)"
                  ],
                  "LoadPage": [
                    "Maybe.Maybe String.String"
                  ],
                  "ModifyEvent": [],
                  "ModifySelectedEvent": [
                    "Event.GroupEvent.GroupEvent"
                  ],
                  "ParticipantsLoaded": [
                    "Result.Result Http.Error (List.List Event.GroupEvent.GroupEventParticipant)"
                  ],
                  "ResponseUpdated": [
                    "Result.Result Http.Error Event.GroupEvent.GroupEvent"
                  ],
                  "SelectEvent": [
                    "String.String"
                  ],
                  "SetResponse": [
                    "Event.GroupEvent.GroupEventResponse"
                  ],
                  "ShowCreateEvent": []
                }
              },
              "Pages.GroupPage.Msg": {
                "args": [],
                "tags": {
                  "AcceptInvite": [
                    "String.String"
                  ],
                  "CreateGroup": [],
                  "DeclineInvite": [
                    "String.String"
                  ],
                  "FetchProfile": [
                    "String.String"
                  ],
                  "GroupCreated": [
                    "Result.Result Http.Error Group.Group.GroupInfo"
                  ],
                  "GroupReadSent": [
                    "Result.Result Http.Error ()"
                  ],
                  "GroupSelected": [
                    "Group.Group.GroupInfo"
                  ],
                  "GroupsLoaded": [
                    "Result.Result Http.Error (List.List Group.Group.GroupInfo)"
                  ],
                  "IgnoreInvite": [
                    "String.String",
                    "String.String"
                  ],
                  "Ignored": [
                    "Result.Result Http.Error ()"
                  ],
                  "InvitesLoaded": [
                    "Result.Result Http.Error (List.List Group.Group.GroupInvite)"
                  ],
                  "LoadPage": [
                    "Maybe.Maybe String.String"
                  ],
                  "ManageInviteDone": [
                    "Result.Result Http.Error ()"
                  ],
                  "MembersLoaded": [
                    "Result.Result Http.Error (List.List Group.Group.GroupMember)"
                  ],
                  "MessageSent": [
                    "Result.Result Http.Error ()"
                  ],
                  "MessageUpdate": [
                    "String.String"
                  ],
                  "MessagesLoaded": [
                    "Result.Result Http.Error (List.List Group.Group.GroupMessage)"
                  ],
                  "NewGroupName": [
                    "String.String"
                  ],
                  "NoOp": [],
                  "OpenCommentBox": [
                    "String.String"
                  ],
                  "SendComment": [
                    "String.String"
                  ],
                  "SendMessage": [],
                  "SendReply": [
                    "String.String",
                    "String.String"
                  ],
                  "SetViewMode": [
                    "Pages.GroupPage.ViewMode"
                  ],
                  "ShowGroupTab": [],
                  "ShowInvitesTab": [],
                  "ShowMembersTab": []
                }
              },
              "Pages.GroupPage.ViewMode": {
                "args": [],
                "tags": {
                  "ShowAll": [],
                  "ShowCenter": [],
                  "ShowLeft": []
                }
              },
              "Pages.HomePage.Msg": {
                "args": [],
                "tags": {
                  "NavigateTo": [
                    "Routing.Helpers.Route"
                  ]
                }
              },
              "Pages.LoginPage.Msg": {
                "args": [],
                "tags": {
                  "ChangeEmail": [
                    "String.String"
                  ],
                  "ChangePassword": [
                    "String.String"
                  ],
                  "FinishLogin": [
                    "Result.Result Http.Error SharedState.LoginResponse"
                  ],
                  "GotoRegistration": [],
                  "StartLogin": []
                }
              },
              "Pages.MyProfilePage.Msg": {
                "args": [],
                "tags": {
                  "AddButtonPressed": [],
                  "CancelUpload": [],
                  "DeletePhoto": [
                    "Profile.Photo.Photo"
                  ],
                  "DoneEditing": [],
                  "DoneSelecting": [],
                  "FileLoaded": [
                    "File.File"
                  ],
                  "FinishSaving": [
                    "Result.Result Http.Error String.String"
                  ],
                  "GotPreview": [
                    "String.String"
                  ],
                  "GotProgress": [
                    "Http.Progress"
                  ],
                  "LoadMyProfile": [],
                  "MovePhotoLeft": [
                    "Profile.Photo.Photo"
                  ],
                  "MovePhotoRight": [
                    "Profile.Photo.Photo"
                  ],
                  "MyProfileLoaded": [
                    "Result.Result Http.Error Profile.Profile.Member"
                  ],
                  "NoOp": [
                    "Basics.Bool"
                  ],
                  "RefreshPhotos": [
                    "Result.Result Http.Error Profile.Profile.Member"
                  ],
                  "RequestFile": [],
                  "SaveChanges": [],
                  "ShowAlbum": [],
                  "ShowProfile": [],
                  "StartUpload": [
                    "Maybe.Maybe File.File"
                  ],
                  "ToggleCity": [
                    "String.String"
                  ],
                  "Update": [
                    "Profile.Profile.Profile"
                  ],
                  "UpdatePerson": [
                    "Pages.MyProfilePage.PersonChoice",
                    "Profile.Profile.Person"
                  ],
                  "UpdateProfileKind": [
                    "String.String"
                  ],
                  "Uploaded": [
                    "Result.Result Http.Error ()"
                  ]
                }
              },
              "Pages.MyProfilePage.PersonChoice": {
                "args": [],
                "tags": {
                  "Person1": [],
                  "Person2": []
                }
              },
              "Pages.ProfileListPage.Msg": {
                "args": [],
                "tags": {
                  "AddButtonPressed": [],
                  "DoneSelecting": [],
                  "FetchProfile": [
                    "String.String"
                  ],
                  "GotSearchResult": [
                    "Result.Result Http.Error (List.List Profile.Profile.Profile)"
                  ],
                  "LoadPage": [],
                  "NoOp": [
                    "Basics.Bool"
                  ],
                  "SearchButtonPressed": [],
                  "SearchUserNameButtonPressed": [],
                  "ToggleCity": [
                    "String.String"
                  ],
                  "ToggleOrientation": [
                    "Profile.Profile.Person1Or2",
                    "Profile.ProfileAttributes.PersonOrientation"
                  ],
                  "UpdateCouplesSelection": [
                    "Profile.Profile.CouplesSelection"
                  ],
                  "UpdateMaxAge": [
                    "Profile.Profile.Person1Or2",
                    "String.String"
                  ],
                  "UpdateMinAge": [
                    "Profile.Profile.Person1Or2",
                    "String.String"
                  ],
                  "UserNameUpdate": [
                    "String.String"
                  ]
                }
              },
              "Pages.ProfilePage.Msg": {
                "args": [],
                "tags": {
                  "ChatInviteDone": [
                    "Result.Result Http.Error Chat.Chat.ChatInviteStatus"
                  ],
                  "ChatInviteStatusLoaded": [
                    "Result.Result Http.Error Chat.Chat.ChatInviteStatus"
                  ],
                  "GroupInviteDone": [
                    "Result.Result Http.Error Group.Group.GroupInvite"
                  ],
                  "GroupSelected": [
                    "Group.Group.GroupInfo"
                  ],
                  "GroupsLoaded": [
                    "Result.Result Http.Error (List.List Group.Group.GroupInfo)"
                  ],
                  "InviteToChat": [],
                  "InviteToGroup": [],
                  "LoadProfile": [
                    "Maybe.Maybe String.String"
                  ],
                  "ProfileLoaded": [
                    "Result.Result Http.Error Profile.Profile.Member"
                  ],
                  "SendChatInvite": [],
                  "SendGroupInvite": [],
                  "ShowAlbumTab": [],
                  "ShowProfileTab": [],
                  "UpdateChatInviteMessage": [
                    "String.String"
                  ],
                  "UpdateGroupInviteMessage": [
                    "String.String"
                  ]
                }
              },
              "Pages.RegistrationPage.Msg": {
                "args": [],
                "tags": {
                  "ChangeEmail": [
                    "String.String"
                  ],
                  "ChangePassword": [
                    "String.String"
                  ],
                  "ChangeRepeatEmail": [
                    "String.String"
                  ],
                  "ChangeRepeatPassword": [
                    "String.String"
                  ],
                  "ChangeUserName": [
                    "String.String"
                  ],
                  "FinishRegistration": [
                    "Result.Result Http.Error String.String"
                  ],
                  "StartRegistration": [],
                  "ToggleAcceptTerms": [
                    "Basics.Bool"
                  ]
                }
              },
              "Pages.ResetPasswordPage.Msg": {
                "args": [],
                "tags": {
                  "ChangeMailAddress": [
                    "String.String"
                  ],
                  "ChangePassword": [
                    "String.String"
                  ],
                  "ChangeRepeatPassword": [
                    "String.String"
                  ],
                  "LoadPage": [
                    "Routing.Helpers.ResetPasswordArgument"
                  ],
                  "PasswordResetSent": [
                    "Result.Result Http.Error String.String"
                  ],
                  "RequestPasswordResetMail": [],
                  "RequestPasswordUpdate": [],
                  "ResetMailSent": [
                    "Result.Result Http.Error ()"
                  ]
                }
              },
              "Profile.Profile.CouplesSelection": {
                "args": [],
                "tags": {
                  "FemaleCouple": [],
                  "MaleCouple": [],
                  "MixedCouple": [],
                  "SingleFemale": [],
                  "SingleMale": []
                }
              },
              "Profile.Profile.Person1Or2": {
                "args": [],
                "tags": {
                  "Person1": [],
                  "Person2": []
                }
              },
              "Profile.ProfileAttributes.BreastSize": {
                "args": [],
                "tags": {
                  "A": [],
                  "B": [],
                  "C": [],
                  "D": [],
                  "E": [],
                  "F": [],
                  "G": [],
                  "S": []
                }
              },
              "Profile.ProfileAttributes.PenisSize": {
                "args": [],
                "tags": {
                  "LargePenis": [],
                  "NormalPenis": [],
                  "SecretPenis": [],
                  "SmallPenis": [],
                  "XLargePenis": []
                }
              },
              "Profile.ProfileAttributes.PersonBody": {
                "args": [],
                "tags": {
                  "Large": [],
                  "Medium": [],
                  "SecretBody": [],
                  "Slender": [],
                  "Small": [],
                  "Strong": []
                }
              },
              "Profile.ProfileAttributes.PersonEthnicity": {
                "args": [],
                "tags": {
                  "African": [],
                  "Arabian": [],
                  "Asian": [],
                  "European": [],
                  "Latin": [],
                  "MultiEthnic": [],
                  "OtherEthnicity": [],
                  "SecretEthnicity": []
                }
              },
              "Profile.ProfileAttributes.PersonEyeColor": {
                "args": [],
                "tags": {
                  "BlueEyes": [],
                  "BrownEyes": [],
                  "GreenEyes": [],
                  "GreyEyes": [],
                  "OtherEyes": [],
                  "SecretEyes": []
                }
              },
              "Profile.ProfileAttributes.PersonHabit": {
                "args": [],
                "tags": {
                  "Never": [],
                  "SecretHabit": [],
                  "Sometimes": [],
                  "Yes": []
                }
              },
              "Profile.ProfileAttributes.PersonMartialStatus": {
                "args": [],
                "tags": {
                  "Apart": [],
                  "Benefits": [],
                  "Married": [],
                  "OtherMartial": [],
                  "Partner": [],
                  "SecretMartial": [],
                  "Single": []
                }
              },
              "Profile.ProfileAttributes.PersonOrientation": {
                "args": [],
                "tags": {
                  "Bi": [],
                  "BiCurious": [],
                  "Hetero": [],
                  "Homo": [],
                  "SecretOrientation": []
                }
              },
              "Profile.ProfileAttributes.PersonSex": {
                "args": [],
                "tags": {
                  "Female": [],
                  "Male": [],
                  "SecretSex": []
                }
              },
              "Profile.ProfileAttributes.ShavedStyle": {
                "args": [],
                "tags": {
                  "BikiniShave": [],
                  "FullShave": [],
                  "HalfShave": [],
                  "NoShave": [],
                  "PornShave": [],
                  "SecretShave": [],
                  "Tassel": []
                }
              },
              "Result.Result": {
                "args": [
                  "error",
                  "value"
                ],
                "tags": {
                  "Err": [
                    "error"
                  ],
                  "Ok": [
                    "value"
                  ]
                }
              },
              "Routing.Helpers.ResetPasswordArgument": {
                "args": [],
                "tags": {
                  "Mail": [
                    "String.String"
                  ],
                  "NoArg": [],
                  "Token": [
                    "String.String"
                  ]
                }
              },
              "Routing.Helpers.Route": {
                "args": [],
                "tags": {
                  "ChatRoute": [
                    "Maybe.Maybe String.String"
                  ],
                  "EventRoute": [
                    "Maybe.Maybe String.String"
                  ],
                  "GroupRoute": [
                    "Maybe.Maybe String.String"
                  ],
                  "HomeRoute": [],
                  "LoginRoute": [],
                  "MyProfileRoute": [],
                  "NotFoundRoute": [],
                  "ProfileListRoute": [],
                  "ProfileRoute": [
                    "Maybe.Maybe String.String"
                  ],
                  "RegistrationRoute": [],
                  "ResetPasswordRoute": [
                    "Routing.Helpers.ResetPasswordArgument"
                  ]
                }
              },
              "Routing.Router.Msg": {
                "args": [],
                "tags": {
                  "Beat": [
                    "Time.Posix"
                  ],
                  "ChatMsg": [
                    "Pages.ChatPage.Msg"
                  ],
                  "EventMsg": [
                    "Pages.EventPage.Msg"
                  ],
                  "GotProgress": [
                    "Http.Progress"
                  ],
                  "GroupMsg": [
                    "Pages.GroupPage.Msg"
                  ],
                  "HomeMsg": [
                    "Pages.HomePage.Msg"
                  ],
                  "LoginMsg": [
                    "Pages.LoginPage.Msg"
                  ],
                  "Logout": [],
                  "MyProfileMsg": [
                    "Pages.MyProfilePage.Msg"
                  ],
                  "NavigateTo": [
                    "Routing.Helpers.Route"
                  ],
                  "NewMessagesChecked": [
                    "Result.Result Http.Error Basics.Int"
                  ],
                  "ProfileListMsg": [
                    "Pages.ProfileListPage.Msg"
                  ],
                  "ProfileMsg": [
                    "Pages.ProfilePage.Msg"
                  ],
                  "RegisterMsg": [
                    "Pages.RegistrationPage.Msg"
                  ],
                  "ResetPasswordMsg": [
                    "Pages.ResetPasswordPage.Msg"
                  ],
                  "UrlChange": [
                    "Url.Url"
                  ]
                }
              },
              "String.String": {
                "args": [],
                "tags": {
                  "String": []
                }
              },
              "Time.Posix": {
                "args": [],
                "tags": {
                  "Posix": [
                    "Basics.Int"
                  ]
                }
              },
              "Time.Zone": {
                "args": [],
                "tags": {
                  "Zone": [
                    "Basics.Int",
                    "List.List Time.Era"
                  ]
                }
              },
              "TimeZone.Error": {
                "args": [],
                "tags": {
                  "NoDataForZoneName": [
                    "String.String"
                  ],
                  "NoZoneName": []
                }
              },
              "Url.Protocol": {
                "args": [],
                "tags": {
                  "Http": [],
                  "Https": []
                }
              }
            }
          }
        },
        "history": [
          {
            "$": "GroupMsg",
            "a": {
              "$": "LoadPage",
              "a": {
                "$": "Nothing"
              }
            }
          },
          {
            "$": "Ok",
            "a": {
              "$": "#2",
              "a": "Europe/Stockholm",
              "b": {
                "$": "Zone",
                "a": 60,
                "b": {
                  "$": "::",
                  "a": {
                    "offset": 60,
                    "start": 35667420
                  },
                  "b": {
                    "$": "::",
                    "a": {
                      "offset": 120,
                      "start": 35365020
                    },
                    "b": {
                      "$": "::",
                      "a": {
                        "offset": 60,
                        "start": 35143260
                      },
                      "b": {
                        "$": "::",
                        "a": {
                          "offset": 120,
                          "start": 34840860
                        },
                        "b": {
                          "$": "::",
                          "a": {
                            "offset": 60,
                            "start": 34619100
                          },
                          "b": {
                            "$": "::",
                            "a": {
                              "offset": 120,
                              "start": 34306620
                            },
                            "b": {
                              "$": "::",
                              "a": {
                                "offset": 60,
                                "start": 34094940
                              },
                              "b": {
                                "$": "::",
                                "a": {
                                  "offset": 120,
                                  "start": 33782460
                                },
                                "b": {
                                  "$": "::",
                                  "a": {
                                    "offset": 60,
                                    "start": 33570780
                                  },
                                  "b": {
                                    "$": "::",
                                    "a": {
                                      "offset": 120,
                                      "start": 33258300
                                    },
                                    "b": {
                                      "$": "::",
                                      "a": {
                                        "offset": 60,
                                        "start": 33046620
                                      },
                                      "b": {
                                        "$": "::",
                                        "a": {
                                          "offset": 120,
                                          "start": 32734140
                                        },
                                        "b": {
                                          "$": "::",
                                          "a": {
                                            "offset": 60,
                                            "start": 32512380
                                          },
                                          "b": {
                                            "$": "::",
                                            "a": {
                                              "offset": 120,
                                              "start": 32209980
                                            },
                                            "b": {
                                              "$": "::",
                                              "a": {
                                                "offset": 60,
                                                "start": 31988220
                                              },
                                              "b": {
                                                "$": "::",
                                                "a": {
                                                  "offset": 120,
                                                  "start": 31685820
                                                },
                                                "b": {
                                                  "$": "::",
                                                  "a": {
                                                    "offset": 60,
                                                    "start": 31464060
                                                  },
                                                  "b": {
                                                    "$": "::",
                                                    "a": {
                                                      "offset": 120,
                                                      "start": 31151580
                                                    },
                                                    "b": {
                                                      "$": "::",
                                                      "a": {
                                                        "offset": 60,
                                                        "start": 30939900
                                                      },
                                                      "b": {
                                                        "$": "::",
                                                        "a": {
                                                          "offset": 120,
                                                          "start": 30627420
                                                        },
                                                        "b": {
                                                          "$": "::",
                                                          "a": {
                                                            "offset": 60,
                                                            "start": 30415740
                                                          },
                                                          "b": {
                                                            "$": "::",
                                                            "a": {
                                                              "offset": 120,
                                                              "start": 30103260
                                                            },
                                                            "b": {
                                                              "$": "::",
                                                              "a": {
                                                                "offset": 60,
                                                                "start": 29881500
                                                              },
                                                              "b": {
                                                                "$": "::",
                                                                "a": {
                                                                  "offset": 120,
                                                                  "start": 29579100
                                                                },
                                                                "b": {
                                                                  "$": "::",
                                                                  "a": {
                                                                    "offset": 60,
                                                                    "start": 29357340
                                                                  },
                                                                  "b": {
                                                                    "$": "::",
                                                                    "a": {
                                                                      "offset": 120,
                                                                      "start": 29054940
                                                                    },
                                                                    "b": {
                                                                      "$": "::",
                                                                      "a": {
                                                                        "offset": 60,
                                                                        "start": 28833180
                                                                      },
                                                                      "b": {
                                                                        "$": "::",
                                                                        "a": {
                                                                          "offset": 120,
                                                                          "start": 28530780
                                                                        },
                                                                        "b": {
                                                                          "$": "::",
                                                                          "a": {
                                                                            "offset": 60,
                                                                            "start": 28309020
                                                                          },
                                                                          "b": {
                                                                            "$": "::",
                                                                            "a": {
                                                                              "offset": 120,
                                                                              "start": 27996540
                                                                            },
                                                                            "b": {
                                                                              "$": "::",
                                                                              "a": {
                                                                                "offset": 60,
                                                                                "start": 27784860
                                                                              },
                                                                              "b": {
                                                                                "$": "::",
                                                                                "a": {
                                                                                  "offset": 120,
                                                                                  "start": 27472380
                                                                                },
                                                                                "b": {
                                                                                  "$": "::",
                                                                                  "a": {
                                                                                    "offset": 60,
                                                                                    "start": 27260700
                                                                                  },
                                                                                  "b": {
                                                                                    "$": "::",
                                                                                    "a": {
                                                                                      "offset": 120,
                                                                                      "start": 26948220
                                                                                    },
                                                                                    "b": {
                                                                                      "$": "::",
                                                                                      "a": {
                                                                                        "offset": 60,
                                                                                        "start": 26726460
                                                                                      },
                                                                                      "b": {
                                                                                        "$": "::",
                                                                                        "a": {
                                                                                          "offset": 120,
                                                                                          "start": 26424060
                                                                                        },
                                                                                        "b": {
                                                                                          "$": "::",
                                                                                          "a": {
                                                                                            "offset": 60,
                                                                                            "start": 26202300
                                                                                          },
                                                                                          "b": {
                                                                                            "$": "::",
                                                                                            "a": {
                                                                                              "offset": 120,
                                                                                              "start": 25899900
                                                                                            },
                                                                                            "b": {
                                                                                              "$": "::",
                                                                                              "a": {
                                                                                                "offset": 60,
                                                                                                "start": 25678140
                                                                                              },
                                                                                              "b": {
                                                                                                "$": "::",
                                                                                                "a": {
                                                                                                  "offset": 120,
                                                                                                  "start": 25365660
                                                                                                },
                                                                                                "b": {
                                                                                                  "$": "::",
                                                                                                  "a": {
                                                                                                    "offset": 60,
                                                                                                    "start": 25153980
                                                                                                  },
                                                                                                  "b": {
                                                                                                    "$": "::",
                                                                                                    "a": {
                                                                                                      "offset": 120,
                                                                                                      "start": 24841500
                                                                                                    },
                                                                                                    "b": {
                                                                                                      "$": "::",
                                                                                                      "a": {
                                                                                                        "offset": 60,
                                                                                                        "start": 24629820
                                                                                                      },
                                                                                                      "b": {
                                                                                                        "$": "::",
                                                                                                        "a": {
                                                                                                          "offset": 120,
                                                                                                          "start": 24317340
                                                                                                        },
                                                                                                        "b": {
                                                                                                          "$": "::",
                                                                                                          "a": {
                                                                                                            "offset": 60,
                                                                                                            "start": 24095580
                                                                                                          },
                                                                                                          "b": {
                                                                                                            "$": "::",
                                                                                                            "a": {
                                                                                                              "offset": 120,
                                                                                                              "start": 23793180
                                                                                                            },
                                                                                                            "b": {
                                                                                                              "$": "::",
                                                                                                              "a": {
                                                                                                                "offset": 60,
                                                                                                                "start": 23571420
                                                                                                              },
                                                                                                              "b": {
                                                                                                                "$": "::",
                                                                                                                "a": {
                                                                                                                  "offset": 120,
                                                                                                                  "start": 23269020
                                                                                                                },
                                                                                                                "b": {
                                                                                                                  "$": "::",
                                                                                                                  "a": {
                                                                                                                    "offset": 60,
                                                                                                                    "start": 23047260
                                                                                                                  },
                                                                                                                  "b": {
                                                                                                                    "$": "::",
                                                                                                                    "a": {
                                                                                                                      "offset": 120,
                                                                                                                      "start": 22744860
                                                                                                                    },
                                                                                                                    "b": {
                                                                                                                      "$": "::",
                                                                                                                      "a": {
                                                                                                                        "offset": 60,
                                                                                                                        "start": 22523100
                                                                                                                      },
                                                                                                                      "b": {
                                                                                                                        "$": "::",
                                                                                                                        "a": {
                                                                                                                          "offset": 120,
                                                                                                                          "start": 22210620
                                                                                                                        },
                                                                                                                        "b": {
                                                                                                                          "$": "::",
                                                                                                                          "a": {
                                                                                                                            "offset": 60,
                                                                                                                            "start": 21998940
                                                                                                                          },
                                                                                                                          "b": {
                                                                                                                            "$": "::",
                                                                                                                            "a": {
                                                                                                                              "offset": 120,
                                                                                                                              "start": 21686460
                                                                                                                            },
                                                                                                                            "b": {
                                                                                                                              "$": "::",
                                                                                                                              "a": {
                                                                                                                                "offset": 60,
                                                                                                                                "start": 21474780
                                                                                                                              },
                                                                                                                              "b": {
                                                                                                                                "$": "::",
                                                                                                                                "a": {
                                                                                                                                  "offset": 120,
                                                                                                                                  "start": 21162300
                                                                                                                                },
                                                                                                                                "b": {
                                                                                                                                  "$": "::",
                                                                                                                                  "a": {
                                                                                                                                    "offset": 60,
                                                                                                                                    "start": 20940540
                                                                                                                                  },
                                                                                                                                  "b": {
                                                                                                                                    "$": "::",
                                                                                                                                    "a": {
                                                                                                                                      "offset": 120,
                                                                                                                                      "start": 20638140
                                                                                                                                    },
                                                                                                                                    "b": {
                                                                                                                                      "$": "::",
                                                                                                                                      "a": {
                                                                                                                                        "offset": 60,
                                                                                                                                        "start": 20416380
                                                                                                                                      },
                                                                                                                                      "b": {
                                                                                                                                        "$": "::",
                                                                                                                                        "a": {
                                                                                                                                          "offset": 120,
                                                                                                                                          "start": 20113980
                                                                                                                                        },
                                                                                                                                        "b": {
                                                                                                                                          "$": "::",
                                                                                                                                          "a": {
                                                                                                                                            "offset": 60,
                                                                                                                                            "start": 19892220
                                                                                                                                          },
                                                                                                                                          "b": {
                                                                                                                                            "$": "::",
                                                                                                                                            "a": {
                                                                                                                                              "offset": 120,
                                                                                                                                              "start": 19579740
                                                                                                                                            },
                                                                                                                                            "b": {
                                                                                                                                              "$": "::",
                                                                                                                                              "a": {
                                                                                                                                                "offset": 60,
                                                                                                                                                "start": 19368060
                                                                                                                                              },
                                                                                                                                              "b": {
                                                                                                                                                "$": "::",
                                                                                                                                                "a": {
                                                                                                                                                  "offset": 120,
                                                                                                                                                  "start": 19055580
                                                                                                                                                },
                                                                                                                                                "b": {
                                                                                                                                                  "$": "::",
                                                                                                                                                  "a": {
                                                                                                                                                    "offset": 60,
                                                                                                                                                    "start": 18843900
                                                                                                                                                  },
                                                                                                                                                  "b": {
                                                                                                                                                    "$": "::",
                                                                                                                                                    "a": {
                                                                                                                                                      "offset": 120,
                                                                                                                                                      "start": 18531420
                                                                                                                                                    },
                                                                                                                                                    "b": {
                                                                                                                                                      "$": "::",
                                                                                                                                                      "a": {
                                                                                                                                                        "offset": 60,
                                                                                                                                                        "start": 18319740
                                                                                                                                                      },
                                                                                                                                                      "b": {
                                                                                                                                                        "$": "::",
                                                                                                                                                        "a": {
                                                                                                                                                          "offset": 120,
                                                                                                                                                          "start": 18007260
                                                                                                                                                        },
                                                                                                                                                        "b": {
                                                                                                                                                          "$": "::",
                                                                                                                                                          "a": {
                                                                                                                                                            "offset": 60,
                                                                                                                                                            "start": 17785500
                                                                                                                                                          },
                                                                                                                                                          "b": {
                                                                                                                                                            "$": "::",
                                                                                                                                                            "a": {
                                                                                                                                                              "offset": 120,
                                                                                                                                                              "start": 17483100
                                                                                                                                                            },
                                                                                                                                                            "b": {
                                                                                                                                                              "$": "::",
                                                                                                                                                              "a": {
                                                                                                                                                                "offset": 60,
                                                                                                                                                                "start": 17261340
                                                                                                                                                              },
                                                                                                                                                              "b": {
                                                                                                                                                                "$": "::",
                                                                                                                                                                "a": {
                                                                                                                                                                  "offset": 120,
                                                                                                                                                                  "start": 16958940
                                                                                                                                                                },
                                                                                                                                                                "b": {
                                                                                                                                                                  "$": "::",
                                                                                                                                                                  "a": {
                                                                                                                                                                    "offset": 60,
                                                                                                                                                                    "start": 16737180
                                                                                                                                                                  },
                                                                                                                                                                  "b": {
                                                                                                                                                                    "$": "::",
                                                                                                                                                                    "a": {
                                                                                                                                                                      "offset": 120,
                                                                                                                                                                      "start": 16424700
                                                                                                                                                                    },
                                                                                                                                                                    "b": {
                                                                                                                                                                      "$": "::",
                                                                                                                                                                      "a": {
                                                                                                                                                                        "offset": 60,
                                                                                                                                                                        "start": 16213020
                                                                                                                                                                      },
                                                                                                                                                                      "b": {
                                                                                                                                                                        "$": "::",
                                                                                                                                                                        "a": {
                                                                                                                                                                          "offset": 120,
                                                                                                                                                                          "start": 15900540
                                                                                                                                                                        },
                                                                                                                                                                        "b": {
                                                                                                                                                                          "$": "::",
                                                                                                                                                                          "a": {
                                                                                                                                                                            "offset": 60,
                                                                                                                                                                            "start": 15688860
                                                                                                                                                                          },
                                                                                                                                                                          "b": {
                                                                                                                                                                            "$": "::",
                                                                                                                                                                            "a": {
                                                                                                                                                                              "offset": 120,
                                                                                                                                                                              "start": 15376380
                                                                                                                                                                            },
                                                                                                                                                                            "b": {
                                                                                                                                                                              "$": "::",
                                                                                                                                                                              "a": {
                                                                                                                                                                                "offset": 60,
                                                                                                                                                                                "start": 15154620
                                                                                                                                                                              },
                                                                                                                                                                              "b": {
                                                                                                                                                                                "$": "::",
                                                                                                                                                                                "a": {
                                                                                                                                                                                  "offset": 120,
                                                                                                                                                                                  "start": 14852220
                                                                                                                                                                                },
                                                                                                                                                                                "b": {
                                                                                                                                                                                  "$": "::",
                                                                                                                                                                                  "a": {
                                                                                                                                                                                    "offset": 60,
                                                                                                                                                                                    "start": 14630460
                                                                                                                                                                                  },
                                                                                                                                                                                  "b": {
                                                                                                                                                                                    "$": "::",
                                                                                                                                                                                    "a": {
                                                                                                                                                                                      "offset": 120,
                                                                                                                                                                                      "start": 14328060
                                                                                                                                                                                    },
                                                                                                                                                                                    "b": {
                                                                                                                                                                                      "$": "::",
                                                                                                                                                                                      "a": {
                                                                                                                                                                                        "offset": 60,
                                                                                                                                                                                        "start": 14106300
                                                                                                                                                                                      },
                                                                                                                                                                                      "b": {
                                                                                                                                                                                        "$": "::",
                                                                                                                                                                                        "a": {
                                                                                                                                                                                          "offset": 120,
                                                                                                                                                                                          "start": 13803900
                                                                                                                                                                                        },
                                                                                                                                                                                        "b": {
                                                                                                                                                                                          "$": "::",
                                                                                                                                                                                          "a": {
                                                                                                                                                                                            "offset": 60,
                                                                                                                                                                                            "start": 13531740
                                                                                                                                                                                          },
                                                                                                                                                                                          "b": {
                                                                                                                                                                                            "$": "::",
                                                                                                                                                                                            "a": {
                                                                                                                                                                                              "offset": 120,
                                                                                                                                                                                              "start": 13269660
                                                                                                                                                                                            },
                                                                                                                                                                                            "b": {
                                                                                                                                                                                              "$": "::",
                                                                                                                                                                                              "a": {
                                                                                                                                                                                                "offset": 60,
                                                                                                                                                                                                "start": 13007580
                                                                                                                                                                                              },
                                                                                                                                                                                              "b": {
                                                                                                                                                                                                "$": "::",
                                                                                                                                                                                                "a": {
                                                                                                                                                                                                  "offset": 120,
                                                                                                                                                                                                  "start": 12745500
                                                                                                                                                                                                },
                                                                                                                                                                                                "b": {
                                                                                                                                                                                                  "$": "::",
                                                                                                                                                                                                  "a": {
                                                                                                                                                                                                    "offset": 60,
                                                                                                                                                                                                    "start": 12483420
                                                                                                                                                                                                  },
                                                                                                                                                                                                  "b": {
                                                                                                                                                                                                    "$": "::",
                                                                                                                                                                                                    "a": {
                                                                                                                                                                                                      "offset": 120,
                                                                                                                                                                                                      "start": 12221340
                                                                                                                                                                                                    },
                                                                                                                                                                                                    "b": {
                                                                                                                                                                                                      "$": "::",
                                                                                                                                                                                                      "a": {
                                                                                                                                                                                                        "offset": 60,
                                                                                                                                                                                                        "start": 11959260
                                                                                                                                                                                                      },
                                                                                                                                                                                                      "b": {
                                                                                                                                                                                                        "$": "::",
                                                                                                                                                                                                        "a": {
                                                                                                                                                                                                          "offset": 120,
                                                                                                                                                                                                          "start": 11697180
                                                                                                                                                                                                        },
                                                                                                                                                                                                        "b": {
                                                                                                                                                                                                          "$": "::",
                                                                                                                                                                                                          "a": {
                                                                                                                                                                                                            "offset": 60,
                                                                                                                                                                                                            "start": 11435100
                                                                                                                                                                                                          },
                                                                                                                                                                                                          "b": {
                                                                                                                                                                                                            "$": "::",
                                                                                                                                                                                                            "a": {
                                                                                                                                                                                                              "offset": 120,
                                                                                                                                                                                                              "start": 11173020
                                                                                                                                                                                                            },
                                                                                                                                                                                                            "b": {
                                                                                                                                                                                                              "$": "::",
                                                                                                                                                                                                              "a": {
                                                                                                                                                                                                                "offset": 60,
                                                                                                                                                                                                                "start": 10910940
                                                                                                                                                                                                              },
                                                                                                                                                                                                              "b": {
                                                                                                                                                                                                                "$": "::",
                                                                                                                                                                                                                "a": {
                                                                                                                                                                                                                  "offset": 120,
                                                                                                                                                                                                                  "start": 10638780
                                                                                                                                                                                                                },
                                                                                                                                                                                                                "b": {
                                                                                                                                                                                                                  "$": "::",
                                                                                                                                                                                                                  "a": {
                                                                                                                                                                                                                    "offset": 60,
                                                                                                                                                                                                                    "start": 10376700
                                                                                                                                                                                                                  },
                                                                                                                                                                                                                  "b": {
                                                                                                                                                                                                                    "$": "::",
                                                                                                                                                                                                                    "a": {
                                                                                                                                                                                                                      "offset": 120,
                                                                                                                                                                                                                      "start": 10114620
                                                                                                                                                                                                                    },
                                                                                                                                                                                                                    "b": {
                                                                                                                                                                                                                      "$": "::",
                                                                                                                                                                                                                      "a": {
                                                                                                                                                                                                                        "offset": 60,
                                                                                                                                                                                                                        "start": 9852540
                                                                                                                                                                                                                      },
                                                                                                                                                                                                                      "b": {
                                                                                                                                                                                                                        "$": "::",
                                                                                                                                                                                                                        "a": {
                                                                                                                                                                                                                          "offset": 120,
                                                                                                                                                                                                                          "start": 9590460
                                                                                                                                                                                                                        },
                                                                                                                                                                                                                        "b": {
                                                                                                                                                                                                                          "$": "::",
                                                                                                                                                                                                                          "a": {
                                                                                                                                                                                                                            "offset": 60,
                                                                                                                                                                                                                            "start": 9328380
                                                                                                                                                                                                                          },
                                                                                                                                                                                                                          "b": {
                                                                                                                                                                                                                            "$": "::",
                                                                                                                                                                                                                            "a": {
                                                                                                                                                                                                                              "offset": 120,
                                                                                                                                                                                                                              "start": 9066300
                                                                                                                                                                                                                            },
                                                                                                                                                                                                                            "b": {
                                                                                                                                                                                                                              "$": "::",
                                                                                                                                                                                                                              "a": {
                                                                                                                                                                                                                                "offset": 60,
                                                                                                                                                                                                                                "start": 8804220
                                                                                                                                                                                                                              },
                                                                                                                                                                                                                              "b": {
                                                                                                                                                                                                                                "$": "::",
                                                                                                                                                                                                                                "a": {
                                                                                                                                                                                                                                  "offset": 120,
                                                                                                                                                                                                                                  "start": 8542140
                                                                                                                                                                                                                                },
                                                                                                                                                                                                                                "b": {
                                                                                                                                                                                                                                  "$": "::",
                                                                                                                                                                                                                                  "a": {
                                                                                                                                                                                                                                    "offset": 60,
                                                                                                                                                                                                                                    "start": 8280060
                                                                                                                                                                                                                                  },
                                                                                                                                                                                                                                  "b": {
                                                                                                                                                                                                                                    "$": "::",
                                                                                                                                                                                                                                    "a": {
                                                                                                                                                                                                                                      "offset": 120,
                                                                                                                                                                                                                                      "start": 8017980
                                                                                                                                                                                                                                    },
                                                                                                                                                                                                                                    "b": {
                                                                                                                                                                                                                                      "$": "::",
                                                                                                                                                                                                                                      "a": {
                                                                                                                                                                                                                                        "offset": 60,
                                                                                                                                                                                                                                        "start": 7755900
                                                                                                                                                                                                                                      },
                                                                                                                                                                                                                                      "b": {
                                                                                                                                                                                                                                        "$": "::",
                                                                                                                                                                                                                                        "a": {
                                                                                                                                                                                                                                          "offset": 120,
                                                                                                                                                                                                                                          "start": 7483740
                                                                                                                                                                                                                                        },
                                                                                                                                                                                                                                        "b": {
                                                                                                                                                                                                                                          "$": "::",
                                                                                                                                                                                                                                          "a": {
                                                                                                                                                                                                                                            "offset": 60,
                                                                                                                                                                                                                                            "start": 7221660
                                                                                                                                                                                                                                          },
                                                                                                                                                                                                                                          "b": {
                                                                                                                                                                                                                                            "$": "::",
                                                                                                                                                                                                                                            "a": {
                                                                                                                                                                                                                                              "offset": 120,
                                                                                                                                                                                                                                              "start": 6959580
                                                                                                                                                                                                                                            },
                                                                                                                                                                                                                                            "b": {
                                                                                                                                                                                                                                              "$": "::",
                                                                                                                                                                                                                                              "a": {
                                                                                                                                                                                                                                                "offset": 60,
                                                                                                                                                                                                                                                "start": 6697500
                                                                                                                                                                                                                                              },
                                                                                                                                                                                                                                              "b": {
                                                                                                                                                                                                                                                "$": "::",
                                                                                                                                                                                                                                                "a": {
                                                                                                                                                                                                                                                  "offset": 120,
                                                                                                                                                                                                                                                  "start": 6435420
                                                                                                                                                                                                                                                },
                                                                                                                                                                                                                                                "b": {
                                                                                                                                                                                                                                                  "$": "::",
                                                                                                                                                                                                                                                  "a": {
                                                                                                                                                                                                                                                    "offset": 60,
                                                                                                                                                                                                                                                    "start": 6173340
                                                                                                                                                                                                                                                  },
                                                                                                                                                                                                                                                  "b": {
                                                                                                                                                                                                                                                    "$": "::",
                                                                                                                                                                                                                                                    "a": {
                                                                                                                                                                                                                                                      "offset": 120,
                                                                                                                                                                                                                                                      "start": 5911260
                                                                                                                                                                                                                                                    },
                                                                                                                                                                                                                                                    "b": {
                                                                                                                                                                                                                                                      "$": "::",
                                                                                                                                                                                                                                                      "a": {
                                                                                                                                                                                                                                                        "offset": 60,
                                                                                                                                                                                                                                                        "start": 5649180
                                                                                                                                                                                                                                                      },
                                                                                                                                                                                                                                                      "b": {
                                                                                                                                                                                                                                                        "$": "::",
                                                                                                                                                                                                                                                        "a": {
                                                                                                                                                                                                                                                          "offset": 120,
                                                                                                                                                                                                                                                          "start": 5397180
                                                                                                                                                                                                                                                        },
                                                                                                                                                                                                                                                        "b": {
                                                                                                                                                                                                                                                          "$": "[]"
                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                      }
                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                  }
                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                              }
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                          }
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                      }
                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                  }
                                                                                                                                                                                                                                }
                                                                                                                                                                                                                              }
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                          }
                                                                                                                                                                                                                        }
                                                                                                                                                                                                                      }
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                  }
                                                                                                                                                                                                                }
                                                                                                                                                                                                              }
                                                                                                                                                                                                            }
                                                                                                                                                                                                          }
                                                                                                                                                                                                        }
                                                                                                                                                                                                      }
                                                                                                                                                                                                    }
                                                                                                                                                                                                  }
                                                                                                                                                                                                }
                                                                                                                                                                                              }
                                                                                                                                                                                            }
                                                                                                                                                                                          }
                                                                                                                                                                                        }
                                                                                                                                                                                      }
                                                                                                                                                                                    }
                                                                                                                                                                                  }
                                                                                                                                                                                }
                                                                                                                                                                              }
                                                                                                                                                                            }
                                                                                                                                                                          }
                                                                                                                                                                        }
                                                                                                                                                                      }
                                                                                                                                                                    }
                                                                                                                                                                  }
                                                                                                                                                                }
                                                                                                                                                              }
                                                                                                                                                            }
                                                                                                                                                          }
                                                                                                                                                        }
                                                                                                                                                      }
                                                                                                                                                    }
                                                                                                                                                  }
                                                                                                                                                }
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                }
                                                                                                                              }
                                                                                                                            }
                                                                                                                          }
                                                                                                                        }
                                                                                                                      }
                                                                                                                    }
                                                                                                                  }
                                                                                                                }
                                                                                                              }
                                                                                                            }
                                                                                                          }
                                                                                                        }
                                                                                                      }
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          },
          {
            "scene": {
              "width": 1279,
              "height": 266
            },
            "viewport": {
              "x": 0,
              "y": 0,
              "width": 1279,
              "height": 266
            }
          },
          {
            "$": "Ok",
            "a": {
              "sessionId": "e950cbc3-a4b8-4690-ac66-25a359d11105",
              "userName": "Mysiga vi 36"
            }
          },
          {
            "$": "GroupMsg",
            "a": {
              "$": "LoadPage",
              "a": {
                "$": "Nothing"
              }
            }
          },
          {
            "$": "GroupMsg",
            "a": {
              "$": "GroupsLoaded",
              "a": {
                "$": "Ok",
                "a": {
                  "$": "::",
                  "a": {
                    "groupId": "6d96e735-9f5b-494f-91a2-3eca518e8475",
                    "groupName": "dd",
                    "lastUpdate": {
                      "$": "Posix",
                      "a": 1557240040475
                    }
                  },
                  "b": {
                    "$": "[]"
                  }
                }
              }
            }
          },
          {
            "$": "GroupMsg",
            "a": {
              "$": "GroupsLoaded",
              "a": {
                "$": "Err",
                "a": {
                  "$": "BadStatus",
                  "a": 404
                }
              }
            }
          },
          {
            "$": "GroupMsg",
            "a": {
              "$": "InvitesLoaded",
              "a": {
                "$": "Err",
                "a": {
                  "$": "BadStatus",
                  "a": 404
                }
              }
            }
          },
          {
            "$": "GroupMsg",
            "a": {
              "$": "InvitesLoaded",
              "a": {
                "$": "Ok",
                "a": {
                  "$": "[]"
                }
              }
            }
          }
        ]
      }
    }
    
    """
