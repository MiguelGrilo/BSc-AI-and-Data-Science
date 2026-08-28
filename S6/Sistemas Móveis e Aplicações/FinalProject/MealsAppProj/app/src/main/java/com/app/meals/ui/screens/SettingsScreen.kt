package com.app.meals.ui.screens

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.Image
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.Icons.Default
import androidx.compose.material.icons.automirrored.filled.MenuBook
import androidx.compose.material.icons.filled.Person
import androidx.compose.material3.Button
import androidx.compose.material3.Card
import androidx.compose.material3.CardDefaults
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.material3.Switch
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.ColorFilter
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.res.dimensionResource
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import com.app.meals.R

@Composable
fun SettingsScreen(
    isDarkMode: Boolean,
    onThemeToggle: (Boolean) -> Unit,
    isPortrait: Boolean,
    modifier: Modifier = Modifier
) {
    Column(
        modifier = modifier
            .fillMaxSize()
            .padding(dimensionResource(R.dimen.dimen_16)),
        horizontalAlignment = Alignment.CenterHorizontally
    ) {
        Text(
            text = stringResource(R.string.settings),
            style = MaterialTheme.typography.titleLarge,
            color = MaterialTheme.colorScheme.onSurface,
            fontWeight = FontWeight.Bold,
            modifier = Modifier
                .padding(bottom = dimensionResource(R.dimen.dimen_16))
                .align(Alignment.Start)
        )

        if (isPortrait) {
            Text(
                text = stringResource(R.string.developed),
                style = MaterialTheme.typography.titleMedium,
                color = MaterialTheme.colorScheme.primary,
                modifier = Modifier
                    .padding(bottom = dimensionResource(R.dimen.dimen_8))
                    .align(Alignment.CenterHorizontally)
            )

            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(dimensionResource(R.dimen.dimen_16))
            ) {
                DeveloperProfileCard(
                    name = stringResource(R.string.miguel_name),
                    number = stringResource(R.string.miguel_number),
                    url = stringResource(R.string.miguel_url),
                    modifier = Modifier.weight(1f)
                )
                DeveloperProfileCard(
                    name = stringResource(R.string.tiago_name),
                    number = stringResource(R.string.tiago_number),
                    url = stringResource(R.string.tiago_url),
                    modifier = Modifier.weight(1f)
                )
            }
            Spacer(modifier = Modifier.height(dimensionResource(R.dimen.dimen_12)))

            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(horizontal = dimensionResource(R.dimen.dimen_16)),
                horizontalArrangement = Arrangement.Center,
                verticalAlignment = Alignment.CenterVertically
            ) {
                GitHubRepo(
                    url = stringResource(R.string.repo_url),
                    modifier = Modifier
                        .weight(1f, fill = false)
                        .padding(
                            vertical = dimensionResource(R.dimen.dimen_12),
                            horizontal = dimensionResource(R.dimen.dimen_8)
                        )
                )
                Spacer(modifier = Modifier.width(dimensionResource(R.dimen.dimen_8)))

                Documentation(
                    url = stringResource(R.string.docs_url),
                    modifier = Modifier
                        .weight(1f, fill = false)
                        .padding(
                            vertical = dimensionResource(R.dimen.dimen_12),
                            horizontal = dimensionResource(R.dimen.dimen_8)
                        )
                )
            }

            Spacer(modifier = Modifier.weight(1f))

            DarkModeToggle(isDarkMode = isDarkMode, onThemeToggle = onThemeToggle)

            Spacer(modifier = Modifier.weight(1f))

            UniversityLogo(isDarkMode = isDarkMode)
        } else {
            Row(
                modifier = Modifier.fillMaxSize(),
                horizontalArrangement = Arrangement.spacedBy(dimensionResource(R.dimen.dimen_24))
            ) {
                Column(
                    modifier = Modifier
                        .weight(2f)
                        .fillMaxHeight(),
                    verticalArrangement = Arrangement.SpaceBetween
                ) {

                    Text(
                        text = stringResource(R.string.developed),
                        style = MaterialTheme.typography.titleMedium,
                        color = MaterialTheme.colorScheme.primary,
                        modifier = Modifier
                            .padding(bottom = dimensionResource(R.dimen.dimen_8))
                            .align(Alignment.CenterHorizontally)
                    )

                    Spacer(modifier = Modifier.weight(1f))

                    Row(
                        modifier = Modifier.fillMaxWidth(),
                        horizontalArrangement = Arrangement.spacedBy(dimensionResource(R.dimen.dimen_16))
                    ) {
                        DeveloperProfileCard(
                            name = stringResource(R.string.miguel_name),
                            number = stringResource(R.string.miguel_number),
                            url = stringResource(R.string.miguel_url),
                            modifier = Modifier.weight(1f)
                        )
                        DeveloperProfileCard(
                            name = stringResource(R.string.tiago_name),
                            number = stringResource(R.string.tiago_number),
                            url = stringResource(R.string.tiago_url),
                            modifier = Modifier.weight(1f)
                        )
                    }
                    Spacer(modifier = Modifier.height(dimensionResource(R.dimen.dimen_0)))

                    Row(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(horizontal = dimensionResource(R.dimen.dimen_16)),
                        horizontalArrangement = Arrangement.Center,
                        verticalAlignment = Alignment.CenterVertically
                    ) {
                        GitHubRepo(
                            url = stringResource(R.string.repo_url),
                            modifier = Modifier
                                .weight(1f, fill = false)
                                .padding(
                                    vertical = dimensionResource(R.dimen.dimen_12),
                                    horizontal = dimensionResource(R.dimen.dimen_8)
                                )
                        )
                        Spacer(modifier = Modifier.width(dimensionResource(R.dimen.dimen_4)))

                        Documentation(
                            url = stringResource(R.string.docs_url),
                            modifier = Modifier
                                .weight(1f, fill = false)
                                .padding(
                                    vertical = dimensionResource(R.dimen.dimen_12),
                                    horizontal = dimensionResource(R.dimen.dimen_8)
                                )
                        )
                    }
                }

                Column(
                    modifier = Modifier
                        .weight(2f)
                        .fillMaxHeight(),
                    horizontalAlignment = Alignment.CenterHorizontally,
                    verticalArrangement = Arrangement.Center
                ) {
                    Spacer(modifier.weight(1f))

                    DarkModeToggle(
                        isDarkMode = isDarkMode,
                        onThemeToggle = onThemeToggle
                    )

                    Spacer(modifier = Modifier.height(dimensionResource(R.dimen.dimen_16)))

                    UniversityLogo(
                        isDarkMode = isDarkMode
                    )
                    Spacer(modifier.weight(1f))
                }
            }
        }
    }
}

@Composable
fun DarkModeToggle(isDarkMode: Boolean, onThemeToggle: (Boolean) -> Unit) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = dimensionResource(R.dimen.dimen_24))
            .background(MaterialTheme.colorScheme.surfaceVariant, shape = RoundedCornerShape(dimensionResource(R.dimen.dimen_12)))
            .padding(dimensionResource(R.dimen.dimen_16)),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.SpaceBetween
    ) {
        Text(
            text = stringResource(R.string.dark_mode),
            style = MaterialTheme.typography.bodyLarge,
            fontWeight = FontWeight.Medium
        )
        Switch(
            checked = isDarkMode,
            onCheckedChange = onThemeToggle
        )
    }
}

@Composable
fun UniversityLogo(
    isDarkMode: Boolean,
    modifier: Modifier = Modifier
) {
    Surface(
        modifier = modifier.padding(bottom = dimensionResource(R.dimen.dimen_8)),
        color = MaterialTheme.colorScheme.primaryContainer.copy(alpha = 0.4f),
        shape = RoundedCornerShape(dimensionResource(R.dimen.dimen_8)),
        border = BorderStroke(
            dimensionResource(R.dimen.dimen_1),
            MaterialTheme.colorScheme.primary.copy(alpha = 0.3f)
        )
    ) {
        Box(
            modifier = Modifier.padding(
                    horizontal = dimensionResource(R.dimen.dimen_12),
                    vertical = dimensionResource(R.dimen.dimen_8)),
            contentAlignment = Alignment.Center
        ) {
            Image(
                painter = painterResource(id = R.drawable.logo_uevora),
                contentDescription = stringResource(R.string.uni),
                modifier = Modifier.height(dimensionResource(R.dimen.dimen_64)),
                colorFilter = if (isDarkMode)
                    ColorFilter.tint(Color.White)
                else null
            )
        }
    }
}

@Composable
fun DeveloperProfileCard(
    name: String,
    number: String,
    url: String,
    modifier: Modifier = Modifier
) {
    val uriHandler = LocalUriHandler.current

    Card(
        modifier = modifier,
        colors = CardDefaults.cardColors(containerColor = MaterialTheme.colorScheme.surfaceVariant)
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(dimensionResource(R.dimen.dimen_8)),
            horizontalAlignment = Alignment.CenterHorizontally
        ) {
            Icon(
                imageVector = Default.Person,
                contentDescription = stringResource(R.string.avatar),
                modifier = Modifier
                    .size(dimensionResource(R.dimen.dimen_80))
                    .clip(CircleShape)
                    .padding(dimensionResource(R.dimen.dimen_8)),
                tint = MaterialTheme.colorScheme.primary
            )

            Text(
                text = name,
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.Bold,
                textAlign = TextAlign.Center
            )
            Text(
                text = number,
                style = MaterialTheme.typography.bodyMedium,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
                textAlign = TextAlign.Center
            )

            Spacer(modifier = Modifier.height(dimensionResource(R.dimen.dimen_4)))

            Button(
                onClick = { uriHandler.openUri(url) },
                contentPadding = PaddingValues(
                    horizontal = dimensionResource(R.dimen.dimen_8),
                    vertical = dimensionResource(R.dimen.dimen_4)
                ),
                shape = RoundedCornerShape(dimensionResource(R.dimen.dimen_8))
            ) {
                Row(
                    verticalAlignment = Alignment.CenterVertically,
                    horizontalArrangement = Arrangement.spacedBy(dimensionResource(R.dimen.dimen_8))
                ) {
                    Icon(
                        painter = painterResource(id = R.drawable.github_icon),
                        contentDescription = stringResource(R.string.github),
                        modifier = Modifier.size(dimensionResource(R.dimen.dimen_16))
                    )
                    Text(
                        text = stringResource(R.string.view_github),
                        style = MaterialTheme.typography.labelMedium
                    )
                }
            }
        }
    }
}

@Composable
fun GitHubRepo(
    url: String,
    modifier: Modifier = Modifier
){
    val uriHandler = LocalUriHandler.current

    Button(
        onClick = { uriHandler.openUri(url) },
        modifier = modifier.fillMaxWidth(),
        contentPadding = PaddingValues(
            horizontal = dimensionResource(R.dimen.dimen_8),
            vertical = dimensionResource(R.dimen.dimen_4)
        ),
        shape = RoundedCornerShape(dimensionResource(R.dimen.dimen_8))
    ) {
        Row(
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(dimensionResource(R.dimen.dimen_8))
        ) {
            Icon(
                painter = painterResource(id = R.drawable.github_icon),
                contentDescription = stringResource(R.string.github),
                modifier = Modifier.size(dimensionResource(R.dimen.dimen_16))
            )
            Text(
                text = stringResource(R.string.view_repo),
                style = MaterialTheme.typography.labelMedium
            )
        }
    }
}

@Composable
fun Documentation(
    url: String,
    modifier: Modifier = Modifier
){
    val uriHandler = LocalUriHandler.current

    Button(
        onClick = { uriHandler.openUri(url) },
        modifier = modifier.fillMaxWidth(),
        contentPadding = PaddingValues(
            horizontal = dimensionResource(R.dimen.dimen_8),
            vertical = dimensionResource(R.dimen.dimen_4)
        ),
        shape = RoundedCornerShape(dimensionResource(R.dimen.dimen_8))
    ) {
        Row(
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(dimensionResource(R.dimen.dimen_8))
        ) {
            Icon(
                imageVector = Icons.AutoMirrored.Filled.MenuBook,
                contentDescription = stringResource(R.string.documentation),
                modifier = Modifier.size(dimensionResource(R.dimen.dimen_16))
            )
            Text(
                text = stringResource(R.string.view_docs),
                style = MaterialTheme.typography.labelMedium
            )
        }
    }
}