package com.app.meals.ui.screens

import android.content.res.Configuration
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.filled.ArrowBack
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.PrimaryTabRow
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Tab
import androidx.compose.material3.Text
import androidx.compose.material3.TopAppBar
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalConfiguration
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.res.dimensionResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import coil.compose.AsyncImage
import com.app.meals.R
import com.app.meals.model.Meal
import com.app.meals.ui.components.ErrorScreen
import com.app.meals.ui.components.LoadingScreen
import com.app.meals.ui.viewModels.DetailsUiState

/**
 * Entry point for the details screen. Handles the different UI states.
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun DetailsScreen(
    detailsUiState: DetailsUiState,
    retryAction: () -> Unit,
    onBackClick: () -> Unit,
    modifier: Modifier = Modifier
) {
    when (detailsUiState) {
        is DetailsUiState.Loading -> {
            Scaffold(
                topBar = { BasicTopAppBar(title = stringResource(R.string.recipe_details), onBackClick = onBackClick) },
                modifier = modifier.fillMaxSize()
            ) { innerPadding ->
                LoadingScreen(modifier = Modifier.padding(innerPadding).fillMaxSize())
            }
        }

        is DetailsUiState.Error -> {
            Scaffold(
                topBar = { BasicTopAppBar(title = stringResource(R.string.recipe_details), onBackClick = onBackClick) },
                modifier = modifier.fillMaxSize()
            ) { innerPadding ->
                ErrorScreen(retryAction = retryAction, modifier = Modifier.padding(innerPadding).fillMaxSize())
            }
        }

        is DetailsUiState.Success -> {
            SuccessDetailScreen(
                meal = detailsUiState.meal,
                onBackClick = onBackClick,
                modifier = modifier.fillMaxSize()
            )
        }
    }
}

/**
 * Helper Composable to avoid duplicating the basic TopAppBar for Loading and Error states.
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun BasicTopAppBar(title: String, onBackClick: () -> Unit) {
    TopAppBar(
        title = { Text(title) },
        navigationIcon = {
            IconButton(onClick = onBackClick) {
                Icon(
                    imageVector = Icons.AutoMirrored.Filled.ArrowBack,
                    contentDescription = stringResource(R.string.back)
                )
            }
        }
    )
}

/**
 * Main stateless view for the meal details.
 * Coordinates the Scaffold, Tabs, and delegates content drawing to smaller Composables.
 */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun SuccessDetailScreen(
    meal: Meal,
    onBackClick: () -> Unit,
    modifier: Modifier = Modifier
) {
    var selectedTabIndex by remember { mutableIntStateOf(0) }
    val tabs = listOf(
        stringResource(R.string.ingredients),
        stringResource(R.string.instructions),
        stringResource(R.string.video)
    )

    val configuration = LocalConfiguration.current
    val isPortrait = configuration.orientation == Configuration.ORIENTATION_PORTRAIT

    Scaffold(
        modifier = modifier,
        topBar = {
            Column {
                Row(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(vertical = dimensionResource(R.dimen.dimen_4)),
                    verticalAlignment = Alignment.CenterVertically
                ) {
                    IconButton(onClick = onBackClick) {
                        Icon(
                            imageVector = Icons.AutoMirrored.Filled.ArrowBack,
                            contentDescription = stringResource(R.string.back)
                        )
                    }

                    Text(
                        text = meal.name,
                        style = MaterialTheme.typography.titleLarge,
                        fontWeight = FontWeight.Bold,
                        maxLines = 1,
                        overflow = TextOverflow.Ellipsis,
                        modifier = Modifier
                            .weight(1f)
                            .padding(end = dimensionResource(R.dimen.dimen_16))
                    )
                }

                if (isPortrait) {
                    PrimaryTabRow(selectedTabIndex = selectedTabIndex) {
                        tabs.forEachIndexed { index, title ->
                            Tab(
                                selected = selectedTabIndex == index,
                                onClick = { selectedTabIndex = index },
                                text = { Text(title) }
                            )
                        }
                    }
                }
            }
        }
    ) { innerPadding ->

        if (isPortrait) {
            Column(
                modifier = Modifier
                    .padding(innerPadding)
                    .fillMaxSize()
                    .verticalScroll(rememberScrollState())
            ) {
                MealImage(
                    imageUrl = meal.imageUrl,
                    modifier = Modifier
                        .fillMaxWidth()
                        .height(dimensionResource(R.dimen.dimen_350))
                )

                Column(modifier = Modifier.padding(dimensionResource(R.dimen.dimen_16))) {
                    MealHeaderInfo(meal = meal)

                    when (selectedTabIndex) {
                        0 -> IngredientsTab(ingredients = meal.ingredients, measures = meal.measures)
                        1 -> InstructionsTab(instructions = meal.instructions, source = meal.source)
                        2 -> VideoTab(videoUrl = meal.videoUrl)
                    }
                }
            }
        } else {
            Row(
                modifier = Modifier
                    .padding(innerPadding)
                    .fillMaxSize()
                    .padding(horizontal = dimensionResource(R.dimen.dimen_16)),
                horizontalArrangement = Arrangement.spacedBy(dimensionResource(R.dimen.dimen_24))
            ) {
                Box(
                    modifier = Modifier
                        .weight(1f)
                        .fillMaxHeight()
                        .padding(bottom = dimensionResource(R.dimen.dimen_16))
                ) {
                    MealImage(
                        imageUrl = meal.imageUrl,
                        modifier = Modifier
                            .fillMaxSize()
                            .clip(MaterialTheme.shapes.large)
                    )
                }

                Column(
                    modifier = Modifier
                        .weight(1.2f)
                        .fillMaxHeight()
                ) {
                    PrimaryTabRow(selectedTabIndex = selectedTabIndex) {
                        tabs.forEachIndexed { index, title ->
                            Tab(
                                selected = selectedTabIndex == index,
                                onClick = { selectedTabIndex = index },
                                text = { Text(title) }
                            )
                        }
                    }

                    Column(
                        modifier = Modifier
                            .fillMaxSize()
                            .verticalScroll(rememberScrollState())
                            .padding(vertical = dimensionResource(R.dimen.dimen_16))
                    ) {
                        MealHeaderInfo(meal = meal)

                        when (selectedTabIndex) {
                            0 -> IngredientsTab(ingredients = meal.ingredients, measures = meal.measures)
                            1 -> InstructionsTab(instructions = meal.instructions, source = meal.source)
                            2 -> VideoTab(videoUrl = meal.videoUrl)
                        }
                    }
                }
            }
        }
    }
}

/**
 * Displays the main recipe image.
 */
@Composable
fun MealImage(imageUrl: String?, modifier: Modifier = Modifier) {
    AsyncImage(
        model = imageUrl,
        contentDescription = stringResource(R.string.meal_image),
        modifier = modifier,
        contentScale = ContentScale.Crop
    )
}

/**
 * Displays only the Category and Country tags.
 */
@Composable
fun MealHeaderInfo(meal: Meal) {
    val tags = listOfNotNull(meal.category, meal.country)
        .joinToString(stringResource(R.string.join_to_string))
        .uppercase()

    if (tags.isNotEmpty()) {
        Text(
            text = tags,
            style = MaterialTheme.typography.labelLarge,
            color = MaterialTheme.colorScheme.primary,
            modifier = Modifier.padding(bottom = dimensionResource(R.dimen.dimen_16))
        )
    }
}

/**
 * Tab Content: Displays a list of ingredients and their corresponding measures.
 */
@Composable
fun IngredientsTab(ingredients: List<String>, measures: List<String>) {
    if (ingredients.isEmpty()) {
        Text(
            text = stringResource(R.string.no_ingredients),
            style = MaterialTheme.typography.bodyLarge
        )
    } else {
        ingredients.forEachIndexed { index, ingredient ->
            val measure = measures.getOrNull(index) ?: ""

            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(vertical = dimensionResource(R.dimen.dimen_12)),
                horizontalArrangement = Arrangement.SpaceBetween
            ) {
                Text(
                    text = ingredient.replaceFirstChar { it.uppercase() },
                    style = MaterialTheme.typography.bodyLarge,
                    modifier = Modifier.weight(1f)
                )
                Text(
                    text = measure,
                    style = MaterialTheme.typography.bodyLarge,
                    fontWeight = FontWeight.Bold,
                    color = MaterialTheme.colorScheme.primary
                )
            }
            HorizontalDivider(color = MaterialTheme.colorScheme.surfaceVariant)
        }
    }
}

/**
 * Tab Content: Displays recipe instructions and the original source link.
 */
@Composable
fun InstructionsTab(instructions: String?, source: String?) {
    val uriHandler = LocalUriHandler.current

    Column {
        Text(
            text = instructions ?: stringResource(R.string.no_inst_avail),
            style = MaterialTheme.typography.bodyLarge,
            modifier = Modifier.padding(bottom = dimensionResource(R.dimen.dimen_16))
        )

        if (!source.isNullOrBlank()) {
            Text(
                text = stringResource(R.string.view_source, source),
                style = MaterialTheme.typography.bodyMedium,
                color = MaterialTheme.colorScheme.primary,
                fontWeight = FontWeight.Bold,
                modifier = Modifier
                    .padding(top = dimensionResource(R.dimen.dimen_16))
                    .clickable { uriHandler.openUri(source) }
            )
        }
    }
}

/**
 * Tab Content: Displays a button to redirect to YouTube.
 */
@Composable
fun VideoTab(videoUrl: String?) {
    val uriHandler = LocalUriHandler.current

    if (videoUrl.isNullOrBlank()) {
        Text(
            text = stringResource(R.string.no_video),
            style = MaterialTheme.typography.bodyLarge,
            modifier = Modifier.padding(top = dimensionResource(R.dimen.dimen_16))
        )
        return
    }

    Button(
        onClick = { uriHandler.openUri(videoUrl) },
        modifier = Modifier
            .fillMaxWidth()
            .padding(top = dimensionResource(R.dimen.dimen_16)),
        shape = MaterialTheme.shapes.medium,
        colors = ButtonDefaults.buttonColors(
            containerColor = MaterialTheme.colorScheme.error
        )
    ) {
        Text(
            text = stringResource(R.string.watch_youtube),
            modifier = Modifier.padding(dimensionResource(R.dimen.dimen_8))
        )
    }
}